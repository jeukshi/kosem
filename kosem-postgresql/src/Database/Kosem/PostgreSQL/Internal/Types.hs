{-# LANGUAGE DuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Types where

import Data.ByteString.Builder (Builder)
import Data.Coerce (coerce)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Classes
import Language.Haskell.TH (Name)
import Language.Haskell.TH.Lift (Lift)

data IsNullable
    = NonNullable
    | Nullable
    deriving (Show, Eq)

newtype PgType = Scalar Identifier
    deriving (Show, Eq, Lift)

newtype Identifier = Identifier Text
    deriving (Show) via Text
    deriving (Eq) via Text
    deriving (IsString) via Text
    deriving (Lift)

identifierToString :: Identifier -> String
identifierToString = T.unpack . coerce

instance ToRawSql Identifier where
    toRawSql :: Identifier -> Builder
    toRawSql = textToBuilder . coerce

data Database = Database
    { name :: Text
    , typesMap :: [(PgType, Name)]
    , tables :: [Table]
    }
    deriving (Show, Eq, Lift)

data Table = Table
    { name :: Identifier
    , columns :: [Column]
    }
    deriving (Show, Eq, Lift)

data Column = Column
    { name :: Identifier
    , typeName :: PgType
    }
    deriving (Show, Eq, Lift)

data TypeInfo
    = TypeInfo PgType IsNullable
    deriving (Show, Eq)
