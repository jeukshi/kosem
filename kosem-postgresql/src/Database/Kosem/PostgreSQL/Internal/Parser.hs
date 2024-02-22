module Database.Kosem.PostgreSQL.Internal.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.NonEmpty qualified as Combinators.NonEmpty
import Control.Monad.Cont (MonadPlus (mzero))
import Data.Char (isNumber)
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Database.Kosem.PostgreSQL.Internal.Ast
import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)
import Text.Pretty.Simple
import Prelude hiding (takeWhile)

data Token = Token SourcePos (STerm ())

symbol :: Text -> Parser Text
symbol = L.symbol' skipWhitespace

comma :: Parser Text
comma = symbol ","

select :: Parser ()
select = pKeyword "select"

join :: Parser ()
join = pKeyword "join"

left :: Parser ()
left = pKeyword "left"

from :: Parser ()
from = pKeyword "from"

whereK :: Parser ()
whereK = pKeyword "where"

on :: Parser ()
on = pKeyword "on"

as :: Parser ()
as = pKeyword "as"

andK :: Parser And
andK = And <$ pKeyword "and"

orK :: Parser Or
orK = Or <$ pKeyword "or"

notK :: Parser Not
notK = Not <$ pKeyword "not"

-- TODO `between symmetric`
betweenK :: Parser Between
betweenK = Between <$ pKeyword "between"

notBetweenOpP :: Parser Operator
notBetweenOpP = lexeme do
    not <- notK
    Op_NotBetween not <$> betweenK

-- equalExpression :: Parser Expression
-- equalExpression =
-- makeOperator (do "=="; notFollowedBy "="; return Equal) notEqualExpression

allPred :: [a -> Bool] -> a -> Bool
allPred ps a = all (\p -> p a) ps

newline :: Parser ()
newline = void C.newline

selectCore :: Parser (STerm ())
selectCore = do
    _ <- skipMany C.spaceChar
    select
    resultColumn <- resultColumnP
    from <- optional do
        from
        From <$> fromItemP
    whereClause <- optional do
        whereK
        Where <$> exprP
    eof
    return $ Select resultColumn from whereClause

fromItemP :: Parser (FromItem ())
fromItemP = lexeme do
    fromItem <-
        choice
            [ fromItemTableNameP
            ]
    tryJoin fromItem
  where
    tryJoin :: FromItem () -> Parser (FromItem ())
    tryJoin lhs = do
        mbJoin <- optional joinTypeP
        case mbJoin of
            Just joinType -> do
                rhs <- fromItemP
                joinCondition <- joinOnP
                let fromItem = FiJoin lhs joinType rhs joinCondition
                tryJoin fromItem
            Nothing -> return lhs

joinTypeP :: Parser JoinType
joinTypeP = lexeme do
    choice
        [ JtJoin <$ join
        , JtLeftJoin <$ (left >> join)
        ]

joinOnP :: Parser (JoinCondition ())
joinOnP = lexeme do
    on
    JcOn <$> exprP

fromItemTableNameP :: Parser (FromItem ())
fromItemTableNameP = lexeme do
    FiTableName <$> tableNameP

tableNameP :: Parser TableName
tableNameP = lexeme do
    first <- satisfy isAlpha

    rest <- takeWhile do
        anyPred [isAlpha, isDigit, isUnderscore]

    return $ TableName (T.cons first rest)

reserved = ["from", "where", "and", "not", "or"]

aliasedExprP :: Parser (AliasedExpr ())
aliasedExprP = lexeme do
    expr <- exprP
    maybeAs <- (As <$) <$> optional as
    case maybeAs of
        Nothing -> do
            optional (lookAhead labelP) >>= \case
                Nothing -> return $ WithoutAlias expr
                Just lookAheadAlias ->
                    if lookAheadAlias `elem` reserved
                        then return $ WithoutAlias expr
                        else do
                            alias <- labelP
                            return $ WithAlias expr alias Nothing
        Just _ -> do
            alias <- labelP
            return $ WithAlias expr alias maybeAs

data Operator
    = Op_And And
    | Op_Or Or
    | Op_LessThan
    | Op_GreaterThan
    | Op_LessThanOrEqualTo
    | Op_GreaterThanOrEqualTo
    | Op_Equal
    | Op_NotEqual NotEqualStyle
    | Op_Between Between
    | Op_NotBetween Not Between

operatorP :: Parser Operator
operatorP = lexeme do
    choice
        [ Op_And <$> andK
        , Op_Or <$> orK
        , Op_NotEqual NotEqualNonStandardStyle <$ try "<>"
        , Op_NotEqual NotEqualStandardStyle <$ try "!="
        , Op_LessThanOrEqualTo <$ try "<="
        , Op_LessThan <$ "<"
        , Op_GreaterThanOrEqualTo <$ try ">="
        , Op_GreaterThan <$ ">"
        , Op_Equal <$ "="
        -- FIXME between has problems with `and`
        -- , Op_Between <$> betweenK
        -- , try notBetweenOpP
        ]

-- \| Op_NotBetween

exprP :: Parser (Expr ())
exprP = lexeme do
    mbNot <- optional $ try notK
    expr <-
        choice
            [ exprLitP
            , exprColP
            ]
    let lhsExpr = case mbNot of
            Nothing -> expr
            Just _ -> ENot Not expr
    (optional . try $ operatorP) >>= \case
        Nothing -> return expr
        Just operator -> combine lhsExpr operator
  where
    combine :: Expr () -> Operator -> Parser (Expr ())
    combine lhs = \cases
        (Op_And and) -> EAnd lhs and <$> exprP
        (Op_Or or) -> EOr lhs or <$> exprP
        Op_LessThan -> ELessThan lhs <$> exprP
        Op_GreaterThan -> EGreaterThan lhs <$> exprP
        Op_LessThanOrEqualTo -> ELessThanOrEqualTo lhs <$> exprP
        Op_GreaterThanOrEqualTo -> EGreaterThanOrEqualTo lhs <$> exprP
        Op_Equal -> EEqual lhs <$> exprP
        (Op_NotEqual style) -> ENotEqual lhs style <$> exprP
        (Op_Between between) ->
            EBetween lhs between <$> exprP <*> andK <*> exprP
        (Op_NotBetween not between) ->
            ENotBetween lhs not between <$> exprP <*> andK <*> exprP

exprLitP :: Parser (Expr ())
exprLitP = lexeme do
    lit <-
        choice
            [ boolLiteralP
            , textLiteralP
            ]
    return $ ELit lit ()

exprColP :: Parser (Expr ())
exprColP = lexeme do
    flip ECol () <$> labelP

exprAndP :: Parser (Expr ())
exprAndP = do
    leftExpr <- exprP
    andK
    EAnd leftExpr And <$> exprP

boolLiteralP :: Parser LiteralValue
boolLiteralP = lexeme do
    s <- C.string "true" <|> C.string "false"
    return $ BoolLiteral s

textLiteralP :: Parser LiteralValue
textLiteralP = lexeme do
    -- FIXME only alphaNum literals
    s <- between (symbol "'") (symbol "'") (many C.alphaNumChar)
    return $ TextLiteral (T.pack s)

resultColumnP :: Parser (NonEmpty (AliasedExpr ()))
resultColumnP = lexeme do
    aliasedExprP `Combinators.NonEmpty.sepBy1` comma -- NonEmpty.map ECol cols

-- case r of
-- "from" -> fail "labelP fail"
-- _ -> return r

-- takeWhileP
{-
simpleLabelFirstChar :: Char -> Bool
simpleLabelFirstChar c = C.alpha c || c == '_'

simpleLabelNextChar :: Char -> Bool
simpleLabelNextChar c = C.alphaNum c || c `elem` [ '-', '/', '_' ]

simpleLabel :: Parser Text
simpleLabel = try do
    first <- satisfy simpleLabelFirstChar

    rest  <- takeWhile simpleLabelNextChar

    let l = Text.cons first rest

    guard (l `notElem` reservedKeywords)

    return l
-}
