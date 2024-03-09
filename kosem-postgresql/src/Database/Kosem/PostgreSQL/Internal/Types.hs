{-# LANGUAGE DuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Types where

import Language.Haskell.TH.Lift (Lift)
import Data.Text (Text)
import Data.String (IsString)
import Language.Haskell.TH (Name)
import Database.Kosem.PostgreSQL.Internal.Classes
import Data.ByteString.Builder (Builder)

data IsNullable
  = NonNullable
  | Nullable
  deriving (Show, Eq)

newtype PgType = PgType { unPgType :: Text }
   deriving Show via Text
   deriving Eq via Text
   deriving IsString via Text
   deriving (Lift)

newtype ColumnName = ColumnName { unColumnName :: Text }
   deriving Show via Text
   deriving Eq via Text
   deriving IsString via Text
   deriving (Lift)

instance ToRawSql ColumnName where
  toRawSql :: ColumnName -> Builder
  toRawSql = textToBuilder . unColumnName

data Database = Database
  { name :: Text
  , typesMap :: [(PgType, Name)]
  , tables :: [Table]
  }
  deriving (Show, Eq, Lift)

data Table = Table
    { name :: Text
    , columns :: [Column]
    }
    deriving (Show, Eq, Lift)

data Column = Column
    { name :: ColumnName
    , typeName :: PgType
    }
    deriving (Show, Eq, Lift)

data SqlType
  = Scalar PgType IsNullable
  deriving (Show, Eq)
