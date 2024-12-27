{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Schema.Internal.Parser where

import Control.Monad (void)
import Data.String (IsString)
import Data.Text (Text)
import Data.Void (Void)
import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Database.Kosem.PostgreSQL.Internal.PgBuiltin
import Database.Kosem.PostgreSQL.Internal.Types
import Database.Kosem.PostgreSQL.Schema.Internal
import Language.Haskell.TH (Name)
import Language.Haskell.TH.Lift (Lift)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L

identifierS :: Parser Identifier
identifierS = lexemeS do
    Identifier <$> dbLabel

databaseK :: Parser ()
databaseK = pKeyword "database"

tableK :: Parser ()
tableK = pKeyword "table"

tableNameP :: Parser Identifier
tableNameP = do
    tableK
    tableName <- identifierS
    return tableName

databaseNameP :: Parser String
databaseNameP = do
    databaseK
    databaseName <- labelP
    return databaseName

-- TODO only for parser
data TableItem
    = TableColumn Identifier PgType
    | TableConstraint
    deriving (Show, Eq, Lift)

tableP :: DatabaseConfig -> Parser Table -- header and list items
tableP databaseConfig = L.nonIndented spaceNewlineP (L.indentBlock spaceNewlineP p)
  where
    p = do
        tableName <- tableNameP

        return (L.IndentSome Nothing (return . Table tableName) (tableItemP databaseConfig))

tableItemP :: DatabaseConfig -> Parser Column
tableItemP databaseConfig = lexemeS do
    columnName <- identifierS <?> "column name"
    -- FIXME assumes Scalar
    typeName <- identifierS <?> "column data type"
    let x = filter (\(n, _) -> n == typeName) databaseConfig.types
    let pgType = head . map snd $ x
    Column columnName pgType <$> isNullableP

isNullableP :: Parser IsNullable
isNullableP = lexemeS do
    optional (C.string "not null") >>= \case
        Nothing -> return Nullable
        Just _ -> return NonNullable

schemaP :: DatabaseConfig -> Parser Database
schemaP databaseConfig = lexeme do
    _ <- skipMany C.spaceChar
    databaseName <- databaseNameP
    tables <- some (tableP databaseConfig)
    return $ Database databaseName [] [] [] [] tables
