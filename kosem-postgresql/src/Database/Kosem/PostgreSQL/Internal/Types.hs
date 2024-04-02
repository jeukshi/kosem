{-# LANGUAGE DuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Types where

import Data.ByteString.Builder (Builder)
import Data.Coerce (coerce)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH (Extension (DuplicateRecordFields), Name)
import Language.Haskell.TH.Lift (Lift)

data IsNullable
    = NonNullable
    | Nullable
    deriving (Show, Eq, Lift)

newtype PgType = Scalar Identifier
    deriving (Show, Eq, Lift)

pgTypePretty :: PgType -> Text
pgTypePretty = \cases
    (Scalar identifier) -> identifierPretty identifier

newtype Identifier = Identifier Text
    deriving (Show) via Text
    deriving (Eq) via Text
    deriving (IsString) via Text
    deriving (Lift)

identifierLength :: Identifier -> Int
identifierLength = T.length . coerce

identifierToString :: Identifier -> String
identifierToString = T.unpack . coerce

identifierToText :: Identifier -> Text
identifierToText = coerce

identifierPretty :: Identifier -> Text
identifierPretty identifier = "‘" <> coerce identifier <> "’"

newtype Operator = Operator Text
    deriving (Show) via Text
    deriving (Eq) via Text
    deriving (IsString) via Text
    deriving (Lift)

operatorLength :: Operator -> Int
operatorLength = T.length . coerce

operatorPretty :: Operator -> Text
operatorPretty operator = "‘" <> coerce operator <> "’"

data Database = Database
    { name :: Text
    , typesMap :: [(Identifier, PgType, Name)]
    , binaryOps :: [(Operator, PgType, PgType, PgType)]
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
    , nullable :: IsNullable
    }
    deriving (Show, Eq, Lift)

data TypeInfo = TypeInfo
    { pgType :: PgType
    , nullable :: IsNullable
    , identifier :: Maybe Identifier
    , hsType :: Name
    }
    deriving (Show, Eq)

data SqlMapping = SqlMapping
    { identifier :: Identifier
    , hsType :: Name
    , nullable :: IsNullable
    }
    deriving (Show, Eq, Lift)
