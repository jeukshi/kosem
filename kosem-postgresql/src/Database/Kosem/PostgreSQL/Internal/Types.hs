{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Types where

import Data.ByteString.Builder (Builder)
import Data.Coerce (coerce)
import Data.Int (Int8)
import Data.List (intersperse)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.TypCategory (TypCategory)
import GHC.Records (HasField (..))
import Language.Haskell.TH (Extension (DuplicateRecordFields), Name)
import Language.Haskell.TH.Lift (Lift)

-- Let's keep this for now.
type Alias = Identifier

data CastContext
    = CastContextExplicit
    | CastContextAssignment
    | CastContextImplicit
    deriving (Show, Eq, Lift)

data PgCast = PgCast
    { source :: PgType
    , target :: PgType
    , castType :: CastContext
    }
    deriving (Show, Eq, Lift)

data IsNullable
    = NonNullable
    | Nullable
    deriving (Show, Eq, Lift)

data IsPrefered
    = Prefered
    | NotPrefered
    deriving (Show, Eq, Lift)

data PgType
    = Base Identifier Identifier TypCategory IsPrefered Int8
    | Pseudo Identifier Identifier TypCategory IsPrefered Int8
    deriving (Show, Eq, Lift)

instance HasField "len" PgType Int8 where
    getField :: PgType -> Int8
    getField (Base _ _ _ _ len) = len
    getField (Pseudo _ _ _ _ len) = len

instance HasField "name" PgType Identifier where
    getField :: PgType -> Identifier
    getField (Base name _ _ _ _) = name
    getField (Pseudo name _ _ _ _) = name

instance HasField "nameSql" PgType Identifier where
    getField :: PgType -> Identifier
    getField (Base _ name _ _ _) = name
    getField (Pseudo _ name _ _ _) = name

pgTypePretty :: PgType -> String
pgTypePretty = \cases
    (Base _ identifier _ _ _) -> identifierPretty identifier
    (Pseudo _ identifier _ _ _) -> identifierPretty identifier

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
    , dbCasts :: [PgCast]
    , typesL :: [PgType]
    , typesMap :: [(PgType, Name)]
    , binaryOps :: [(Operator, PgType, PgType, PgType)]
    , unaryOps :: [(Operator, PgType, PgType)]
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
    , len :: Int8
    }
    deriving (Show, Eq, Lift)
