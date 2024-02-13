{-# LANGUAGE DuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Schema.Internal.Parser where

import Control.Monad (void)
import Data.Text (Text)
import Data.Void (Void)
import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Database.Kosem.PostgreSQL.Schema.Internal
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C
import Text.Megaparsec.Char.Lexer qualified as L
import Data.String (IsString)
import Language.Haskell.TH.Lift (Lift)

labelS :: Parser Text
labelS = lexemeS dbLabel

databaseK :: Parser ()
databaseK = pKeyword "database"

tableK :: Parser ()
tableK = pKeyword "table"

tableNameP :: Parser Text
tableNameP = do
    tableK
    tableName <- labelS
    return tableName


databaseNameP :: Parser Text
databaseNameP = do
    databaseK
    databaseName <- labelP
    return databaseName

data TableItem
    = TableColumn ColumnName PgType
    | TableConstraint
    deriving (Show, Eq, Lift)

newtype ColumnName = ColumnName Text
   deriving Show via Text
   deriving Eq via Text
   deriving IsString via Text
   deriving (Lift)

newtype PgType = PgType Text
   deriving Show via Text
   deriving Eq via Text
   deriving IsString via Text
   deriving (Lift)

data Table = Table
    { name :: Text
    , tableItem :: [TableItem]
    }
    deriving (Show, Eq, Lift)

data Database = Database
  { name :: Text
  , tables :: [Table]
  }
  deriving (Show, Eq, Lift)

tableP :: Parser Table -- header and list items
tableP = L.nonIndented spaceNewlineP (L.indentBlock spaceNewlineP p)
  where
    p = do
        tableName <- tableNameP

        return (L.IndentSome Nothing (return . Table tableName) tableItemP)

tableItemP :: Parser TableItem
tableItemP = lexemeS do
    columnName <- ColumnName <$> labelS <?> "column name or table constraint"
    pgType <- PgType <$> labelS <?> "column data type"
    return $ TableColumn columnName pgType

schemaP :: Parser Database
schemaP = lexeme do
  _ <- skipMany C.spaceChar
  databaseName <- databaseNameP
  tables <- some tableP
  return $ Database databaseName tables
