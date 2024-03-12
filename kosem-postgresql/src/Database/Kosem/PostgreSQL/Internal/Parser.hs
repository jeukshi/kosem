module Database.Kosem.PostgreSQL.Internal.Parser where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr (
    Operator (..),
    makeExprParser,
 )
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
import Database.Kosem.PostgreSQL.Internal.Diagnostics
import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Database.Kosem.PostgreSQL.Internal.Types
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)
import Text.Pretty.Simple
import Prelude hiding (takeWhile)

parse
    :: Text
    -> Either CompileError (STerm ())
parse input = do
    let state =
            State
                { stateInput = input
                , stateOffset = 0
                , statePosState = initPosState input
                , stateParseErrors = []
                }
    let (_, res) = runParser' selectCore state
    case res of
        Left e -> do
            let firstErr = NonEmpty.head (bundleErrors e)
            let p = MkP $ errorOffset firstErr
            let errMsg = parseErrorTextPretty firstErr
            Left $ ParseError p errMsg
        Right r -> Right r

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

tableNameP :: Parser Identifier
tableNameP = lexeme do
    first <- satisfy isAlpha

    rest <- takeWhile do
        anyPred [isAlpha, isDigit, isUnderscore]

    return $ Identifier (T.cons first rest)

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
                            alias <- identifierP
                            return $ WithAlias expr alias Nothing
        Just _ -> do
            alias <- identifierP
            return $ WithAlias expr alias maybeAs

parensExprP :: Parser (Expr ())
parensExprP = lexeme do
    p1 <- getP
    expr <- between (symbol "(") (symbol ")") exprP
    p2 <- getP
    return $ EParens p1 expr p2 ()

termP :: Parser (Expr ())
termP = lexeme do
    choice
        [ parensExprP
        , exprLitP
        , exprColP
        , paramMaybeP
        , paramP
        ]

identifierP :: Parser Identifier
identifierP = Identifier <$> labelP

getP :: Parser P
getP = MkP <$> getOffset

paramP :: Parser (Expr ())
paramP = lexeme do
    p <- getP
    symbol ":"
    (EParam p 0 <$> identifierP) <*> pure ()

paramMaybeP :: Parser (Expr ())
paramMaybeP = lexeme do
    p <- getP
    symbol ":?"
    (EParamMaybe p 0 <$> identifierP) <*> pure ()

pgCastP :: Parser Identifier
pgCastP = lexeme do
    symbol "::"
    identifierP

operatorP :: Text -> Parser Database.Kosem.PostgreSQL.Internal.Types.Operator
operatorP sym = lexeme do
    Operator <$> symbol sym

anyOperatorP :: Parser Database.Kosem.PostgreSQL.Internal.Types.Operator
anyOperatorP = lexeme do
    operator <- T.pack <$> some allowedSymbols
    when (operator == "=>") do
        fail "=> cannot be used as an operator name"
    -- TODO implement other restictions
    -- https://www.postgresql.org/docs/current/sql-createoperator.html
    return $ Operator operator
  where
    allowedSymbols :: Parser Char
    allowedSymbols =
        choice
            [ C.char '+'
            , C.char '-'
            , C.char '*'
            , C.char '/'
            , C.char '<'
            , C.char '>'
            , C.char '='
            , C.char '~'
            , C.char '!'
            , C.char '@'
            , C.char '#'
            , C.char '%'
            , C.char '^'
            , C.char '&'
            , C.char '|'
            , C.char '`'
            , C.char '?'
            ]

binOpP :: Text -> Parser (Expr () -> Expr () -> Expr ())
binOpP sym = mkBinOp <$> getP <*> operatorP sym
  where
    mkBinOp p operator lhs rhs =
        EBinOp p lhs operator rhs ()

anyBinOpP :: Parser (Expr () -> Expr () -> Expr ())
anyBinOpP = lexeme do
    mkBinOp <$> getP <*> anyOperatorP
  where
    mkBinOp p operator lhs rhs =
        EBinOp p lhs operator rhs ()

exprP :: Parser (Expr ())
exprP = makeExprParser termP operatorsTable
  where
    operatorsTable =
        -- TODO precedence:
        -- https://www.postgresql.org/docs/current/sql-syntax-lexical.html#SQL-PRECEDENCE
        [ [Postfix do mkCast <$> lexeme getP <*> pgCastP <*> pure ()]
        , [InfixL do binOpP "^"]
        ,
            [ InfixL do binOpP "*"
            , InfixL do binOpP "/"
            , InfixL do binOpP "%"
            ]
        ,
            [ InfixL do binOpP "+"
            , InfixL do binOpP "-"
            ]
        , [InfixL do anyBinOpP]
        ,
            [ Postfix do
                mkBetween
                    <$> lexeme getP
                    <*> betweenK
                    <*> termP
                    <*> andK
                    <*> termP
            , Postfix do
                mkNotBetween
                    <$> lexeme getP
                    <*> notK
                    <*> betweenK
                    <*> termP
                    <*> andK
                    <*> termP
            ]
        ,
            [ InfixL do binOpP "<>"
            , InfixL do binOpP "!="
            , InfixL do binOpP "<="
            , InfixL do binOpP "<"
            , InfixL do binOpP ">="
            , InfixL do binOpP ">"
            , InfixL do binOpP "="
            ]
        , [Prefix do ENot <$> lexeme getP <*> notK]
        , [InfixL do mkAnd <$> lexeme getP <*> andK]
        , [InfixL do mkOr <$> lexeme getP <*> orK]
        ]
    mkCast p text ty expr = EPgCast p expr text ty
    mkBetween p between rhs1 and rhs2 lhs =
        EBetween p lhs between rhs1 and rhs2

    mkNotBetween p not between rhs1 and rhs2 lhs =
        ENotBetween p lhs not between rhs1 and rhs2
    mkAnd p and lhs = EAnd p lhs and
    mkOr p and lhs = EOr p lhs and

exprLitP :: Parser (Expr ())
exprLitP = lexeme do
    p <- getP
    lit <-
        choice
            [ boolLiteralP
            , textLiteralP
            ]
    return $ ELit p lit ()

exprColP :: Parser (Expr ())
exprColP = lexeme do
    p <- getP
    flip (ECol p) () <$> (Identifier <$> labelP)

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
