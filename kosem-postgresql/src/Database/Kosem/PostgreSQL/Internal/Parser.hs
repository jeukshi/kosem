module Database.Kosem.PostgreSQL.Internal.Parser where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
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

parens :: Parser a -> Parser a
parens p = lexeme do
    between (symbol "(") (symbol ")") p

termP :: Parser (Expr ())
termP = lexeme do
    choice
        [ parens exprP
        , exprLitP
        , exprColP
        ]

pgCastP :: Parser Text
pgCastP = lexeme do
    symbol "::"
    labelP

exprP :: Parser (Expr ())
exprP = makeExprParser termP operatorsTable
  where
    operatorsTable :: [[Operator Parser (Expr ())]]
    operatorsTable =
        -- TODO precedence:
        -- https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-PRECEDENCE
        [ [Postfix do flip EPgCast <$> pgCastP]
        ,
            [ InfixL do flip ENotEqual NotEqualNonStandardStyle <$ lexeme "<>"
            , InfixL do flip ENotEqual NotEqualStandardStyle <$ lexeme "!="
            , InfixL do ELessThanOrEqualTo <$ lexeme "<="
            , InfixL do ELessThan <$ lexeme "<"
            , InfixL do EGreaterThanOrEqualTo <$ lexeme ">="
            , InfixL do EGreaterThan <$ lexeme ">"
            , InfixL do EEqual <$ lexeme "="
            ]
        ,
            [ Postfix do
                mkBetween <$> betweenK <*> termP <*> andK <*> termP
            , Postfix do
                mkNotBetween <$> notK <*> betweenK <*> termP <*> andK <*> termP
            ]
        , [Prefix do ENot <$> notK]
        , [InfixL do flip EAnd <$> andK]
        , [InfixL do flip EOr <$> orK]
        ]
    mkBetween between rhs1 and rhs2 lhs =
        EBetween lhs between rhs1 and rhs2

    mkNotBetween not between rhs1 and rhs2 lhs =
        ENotBetween lhs not between rhs1 and rhs2

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
