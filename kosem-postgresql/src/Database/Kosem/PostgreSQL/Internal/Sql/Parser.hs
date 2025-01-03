module Database.Kosem.PostgreSQL.Internal.Sql.Parser where

import Bluefin.Eff
import Bluefin.Exception (Exception, throw)
import Control.Monad (void, when)
import Control.Monad.Combinators.Expr (
    Operator (..),
    makeExprParser,
 )
import Control.Monad.Combinators.NonEmpty qualified as Combinators.NonEmpty
import Data.Char (isNumber)
import Data.Functor (($>), (<&>))
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Text qualified as T
import Data.Void (Void)
import Database.Kosem.PostgreSQL.Internal.Diagnostics
import Database.Kosem.PostgreSQL.Internal.P
import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Database.Kosem.PostgreSQL.Internal.Sql.Ast
import Database.Kosem.PostgreSQL.Internal.Types
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Debug (dbg)
import Text.Pretty.Simple
import Text.Read (readMaybe)
import Prelude hiding (takeWhile)

run
    :: (e :> es)
    => Exception CompileError e
    -> String
    -> Eff es (STerm ())
run ex input = case parse input of
    (Left e) -> throw ex e
    (Right ast) -> return ast

parse
    :: String
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
            -- TODO maybe we can allow longer DiagnosticSpans
            Left $ ParseError p errMsg
        Right r -> Right r

symbol :: String -> Parser String
symbol = L.symbol' skipWhitespace

comma :: Parser String
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
    p <- getP
    tableName <- tableNameP
    maybeAs <- (As <$) <$> optional as
    maybeAliasP >>= \case
        Nothing -> return $ FiTableName p tableName tableName
        (Just alias) -> return $ FiTableName p tableName alias

maybeAliasP :: Parser (Maybe Alias)
maybeAliasP = do
    maybeAs <- (As <$) <$> optional as
    case maybeAs of
        Nothing -> do
            optional (lookAhead labelP) >>= \case
                Nothing -> return Nothing
                Just lookAheadAlias ->
                    if lookAheadAlias `elem` reserved
                        then return Nothing
                        else Just <$> identifierP
        Just _ -> do Just <$> identifierP

tableNameP :: Parser Identifier
tableNameP = lexeme do
    first <- satisfy isAlpha

    rest <- takeWhile do
        anyPred [isAlpha, isDigit, isUnderscore]

    return $ Identifier (first : rest)

reserved =
    [ "from"
    , "where"
    , "and"
    , "not"
    , "or"
    , "on"
    , "join"
    ]

aliasedExprP :: Parser (AliasedExpr ())
aliasedExprP = lexeme do
    expr <- exprP
    maybeAliasP >>= \case
        Nothing -> return $ WithoutAlias expr
        (Just alias) -> return $ WithAlias expr alias

parensExprP :: Parser (Expr ())
parensExprP = lexeme do
    p1 <- getP
    _ <- symbol "("
    expr <- exprP
    p2 <- lexeme getP
    _ <- symbol ")"
    -- \| Move p2 by one to match a position of ')'.
    return $ EParens p1 expr (p2 `movePby` 1) ()

functionExprP :: Parser (Expr ())
functionExprP = lexeme do
    p <- getP
    identifier <- identifierP
    _ <- symbol "("
    args <- (lexeme do exprP) `sepBy1` lexeme do ","
    p2 <- lexeme getP
    _ <- symbol ")"
    return $ EFunction p identifier args ()

termP :: Parser (Expr ())
termP = lexeme do
    choice
        [ parensExprP
        , try functionExprP
        , exprLiteralP
        , exprColP
        , paramMaybeP
        , paramP
        ]

identifierP :: Parser Identifier
identifierP = Identifier <$> labelP

hsIdentifierP :: Parser HsIdentifier
hsIdentifierP = do
    idPart <- identifierP
    mbIdPart <- many do
        _ <- symbol "."
        identifierP
    return $ MkHsIdentifier idPart mbIdPart

getP :: Parser P
getP = MkP <$> getOffset

paramP :: Parser (Expr ())
paramP = lexeme do
    p <- getP
    symbol ":"
    hsId <- hsIdentifierP
    return $ EParam p hsId ()

paramMaybeP :: Parser (Expr ())
paramMaybeP = lexeme do
    p <- getP
    symbol ":?"
    hsId <- hsIdentifierP
    return $ EParamMaybe p hsId ()

pgCastP :: Parser (P, Identifier)
pgCastP = lexeme do
    symbol "::"
    lexeme do
        p <- getP
        i <- identifierP
        pure (p, i)

operatorP :: String -> Parser Database.Kosem.PostgreSQL.Internal.Types.Operator
operatorP sym = lexeme do
    Operator <$> symbol sym

anyOperatorP :: Parser Database.Kosem.PostgreSQL.Internal.Types.Operator
anyOperatorP = lexeme do
    operator <- some allowedSymbols
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
            <?> "<operator>"

binOpP :: String -> Parser (Expr () -> Expr () -> Expr ())
binOpP sym = mkBinOp <$> getP <*> operatorP sym
  where
    mkBinOp p operator lhs rhs =
        EBinOp p lhs operator rhs ()

unaryOpP :: String -> Parser (Expr () -> Expr ())
unaryOpP sym = lexeme do
    mkUnaryOp <$> lexeme getP <*> operatorP sym
  where
    mkUnaryOp p operator rhs =
        EUnaryOp p operator rhs ()

anyUnaryOpP :: Parser (Expr () -> Expr ())
anyUnaryOpP = lexeme do
    mkUnaryOp <$> lexeme getP <*> anyOperatorP
  where
    mkUnaryOp p operator rhs =
        EUnaryOp p operator rhs ()

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
        [
            [ Postfix do
                mkCast
                    <$> lexeme getP
                    <*> pgCastP
                    <*> pure ()
            ]
        ,
            [ Prefix do unaryOpP "+"
            , Prefix do unaryOpP "-"
            ]
        , [InfixL do binOpP "^" <?> "<operator>"]
        ,
            [ InfixL do binOpP "*" <?> "<operator>"
            , InfixL do binOpP "/" <?> "<operator>"
            , InfixL do binOpP "%" <?> "<operator>"
            ]
        ,
            [ InfixL do binOpP "+" <?> "<operator>"
            , InfixL do binOpP "-" <?> "<operator>"
            ]
        ,
            [ InfixL do anyBinOpP
            , Prefix do anyUnaryOpP
            ]
        ,
            [ Postfix do
                mkBetween
                    <$> lexeme getP
                    <* pKeyword "between"
                    <*> termP
                    <* pKeyword "and"
                    <*> termP
            , Postfix do
                mkNotBetween
                    <$> lexeme getP
                    <* pKeyword "not"
                    <* pKeyword "between"
                    <*> termP
                    <* pKeyword "and"
                    <*> termP
            ]
        ,
            [ InfixL do binOpP "<>" <?> "<operator>"
            , InfixL do binOpP "!=" <?> "<operator>"
            , InfixL do binOpP "<=" <?> "<operator>"
            , InfixL do binOpP "<" <?> "<operator>"
            , InfixL do binOpP ">=" <?> "<operator>"
            , InfixL do binOpP ">" <?> "<operator>"
            , InfixL do binOpP "=" <?> "<operator>"
            ]
        , [Prefix do ENot <$> lexeme getP <* pKeyword "not"]
        ,
            [ InfixL do EAnd <$> lexeme getP <* pKeyword "and"
            , -- This feeld a bit hacky.
              Postfix do foldr1 (.) <$> some (try mkGuardedMaybeAnd <|> mkGuardedBoolAnd)
            ]
        , [InfixL do EOr <$> lexeme getP <* pKeyword "or"]
        ]
    mkCast p1 (p2, identifier) ty expr = EPgCast p1 expr p2 identifier ty
    mkBetween p rhs1 rhs2 lhs =
        EBetween p lhs rhs1 rhs2

    mkNotBetween p rhs1 rhs2 lhs =
        ENotBetween p lhs rhs1 rhs2

    mkGuardedBoolAnd = lexeme do
        p1 <- getP
        symbol ":"
        identifier <- hsIdentifierP
        pOpen <- lexeme getP
        _ <- symbol "{and"
        innerExpr <- exprP
        pClose <- lexeme getP
        _ <- symbol "}"
        return $ mk p1 identifier pOpen innerExpr pClose
      where
        mk p1 identifier pOpen innerExpr pClose lhs =
            EGuardedBoolAnd lhs p1 identifier pOpen innerExpr pClose

    mkGuardedMaybeAnd = lexeme do
        p1 <- getP
        symbol ":?"
        identifier <- hsIdentifierP
        pOpen <- lexeme getP
        _ <- symbol "{and"
        innerExpr <- exprP
        pClose <- lexeme getP
        _ <- symbol "}"
        return $ mk p1 identifier pOpen innerExpr pClose
      where
        mk p1 identifier pOpen innerExpr pClose lhs =
            EGuardedMaybeAnd lhs p1 identifier pOpen innerExpr pClose

exprLiteralP :: Parser (Expr ())
exprLiteralP = lexeme do
    p <- getP
    lit <-
        choice
            [ boolLiteralP
            , integerLiteralP <?> "<number>"
            , stringLiteralP <?> "<string>"
            ]
    return $ ELiteral p lit ()

exprColP :: Parser (Expr ())
exprColP = lexeme do
    p <- getP
    idPart <- identifierP
    mbIdPart <- optional do
        _ <- symbol "."
        identifierP
    case mbIdPart of
        (Just identifier) -> return $ ECol p (Just idPart) identifier ()
        Nothing -> return $ ECol p Nothing idPart ()

integerLiteralP :: Parser LiteralValue
integerLiteralP = lexeme do
    str <- some C.digitChar
    case readMaybe @Integer str of
        Just val -> return $ IntegerLiteral val
        Nothing -> error $ "TODO cannot parse as integer: " <> str

boolLiteralP :: Parser LiteralValue
boolLiteralP = lexeme do
    s <- C.string "true" <|> C.string "false"
    return $ BoolLiteral s

stringLiteralP :: Parser LiteralValue
stringLiteralP = lexeme do
    -- FIXME only alphaNum literals
    s <- between (symbol "'") (symbol "'") (many C.alphaNumChar)
    return $ StringLiteral s

resultColumnP :: Parser (NonEmpty (AliasedExpr ()))
resultColumnP = lexeme do
    aliasedExprP `Combinators.NonEmpty.sepBy1` comma -- NonEmpty.map ECol cols
