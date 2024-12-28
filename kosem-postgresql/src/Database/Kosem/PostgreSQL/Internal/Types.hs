{-# LANGUAGE DuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Types where

import Data.ByteString.Builder (Builder)
import Data.Coerce (coerce)
import Data.List (intersperse)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.TypCategory (TypCategory)
import Language.Haskell.TH (Extension (DuplicateRecordFields), Name)
import Language.Haskell.TH.Lift (Lift)

-- Let's keep this for now.
type Alias = Identifier

data IsNullable
    = NonNullable
    | Nullable
    deriving (Show, Eq, Lift)

data IsPrefered
    = Prefered
    | NotPrefered
    deriving (Show, Eq, Lift)

data PgType
    = Base Identifier Identifier TypCategory IsPrefered
    | Pseudo Identifier Identifier TypCategory IsPrefered
    deriving (Show, Eq, Lift)

pgTypePretty :: PgType -> String
pgTypePretty = \cases
    (Base _ identifier _ _) -> identifierPretty identifier
    (Pseudo _ identifier _ _) -> identifierPretty identifier

newtype Identifier = Identifier String
    deriving (Show) via String
    deriving (Eq) via String
    deriving (IsString) via String
    deriving (Lift)

data HsIdentifier = MkHsIdentifier
    { hsIdentifier :: Identifier
    , hiRecordDot :: [Identifier]
    }
    deriving (Show, Eq)

identifierLength :: Identifier -> Int
identifierLength (Identifier t) = length t

hsIdentifierLength :: HsIdentifier -> Int
hsIdentifierLength = \cases
    (MkHsIdentifier i []) -> identifierLength i
    (MkHsIdentifier i is) -> do
        -- \| +1 for dot between identifiers: `myRecord.myField.myOtherField`.
        let rest = (+ 1) . sum . intersperse 1 . map identifierLength $ is
        identifierLength i + rest

identifierToString :: Identifier -> String
identifierToString (Identifier t) = t

identifierToText :: Identifier -> Text
identifierToText (Identifier t) = T.pack t

identifierPretty :: Identifier -> String
identifierPretty (Identifier t) = "‘" <> t <> "’"

aliasedIdentifierPretty :: Alias -> Identifier -> String
aliasedIdentifierPretty (Identifier a) (Identifier t) =
    "‘" <> a <> "." <> t <> "’"

newtype Operator = Operator String
    deriving (Show) via String
    deriving (Eq) via String
    deriving (IsString) via String
    deriving (Lift)

operatorLength :: Operator -> Int
operatorLength (Operator o) = length o

operatorPretty :: Operator -> String
operatorPretty operator = "‘" <> coerce operator <> "’"

data Database = Database
    { name :: String
    , typesL :: [(Identifier, PgType)]
    , typesMap :: [(PgType, Name)]
    , binaryOps :: [(Operator, PgType, PgType, PgType)]
    , functions :: [(Identifier, [PgType], PgType)]
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
    }
    deriving (Show, Eq)

data SqlMapping = SqlMapping
    { identifier :: Identifier
    , hsType :: Name
    , nullable :: IsNullable
    }
    deriving (Show, Eq, Lift)
