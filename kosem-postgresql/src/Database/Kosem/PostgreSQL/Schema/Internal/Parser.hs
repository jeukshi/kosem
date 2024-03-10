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

databaseNameP :: Parser Text
databaseNameP = do
  databaseK
  databaseName <- labelP
  return databaseName

-- TODO only for parser
data TableItem
  = TableColumn Identifier PgType
  | TableConstraint
  deriving (Show, Eq, Lift)

tableP :: Parser Table -- header and list items
tableP = L.nonIndented spaceNewlineP (L.indentBlock spaceNewlineP p)
 where
  p = do
    tableName <- tableNameP

    return (L.IndentSome Nothing (return . Table tableName) tableItemP)

tableItemP :: Parser Column
tableItemP = lexemeS do
  columnName <- identifierS <?> "column name"
  -- FIXME assumes Scalar
  pgType <- Scalar <$> identifierS <?> "column data type"
  return $ Column columnName pgType

isNullableP :: Parser IsNullable
isNullableP = undefined

schemaP :: Parser Database
schemaP = lexeme do
  _ <- skipMany C.spaceChar
  databaseName <- databaseNameP
  tables <- some tableP
  return $ Database databaseName [] [] tables
