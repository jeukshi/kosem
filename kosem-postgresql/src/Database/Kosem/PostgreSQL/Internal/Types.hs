{-# LANGUAGE DuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Types where

import Data.ByteString.Builder (Builder)
import Data.Coerce (coerce)
import Data.List (intersperse)
import Data.String (IsString)
import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH (Extension (DuplicateRecordFields), Name)
import Language.Haskell.TH.Lift (Lift)

-- Let's keep this for now.
type Alias = Identifier

-- TODO this might not be right, as custom categories can be created
data PgTypCategory
    = TcA -- 'A': Array types
    | TcB -- 'B': Boolean types
    | TcC -- 'C': Composite types
    | TcD -- 'D': Date/time types
    | TcE -- 'E': Enum types
    | TcG -- 'G': Geometric types
    | TcI -- 'I': Network address types
    | TcN -- 'N': Numeric types
    | TcP -- 'P': Pseudo types
    | TcR -- 'R': Range types
    | TcS -- 'S': String types
    | TcT -- 'T': Timespan types
    | TcU -- 'U': User-defined types
    | TcV -- 'V': Bit-string types
    | TcX -- 'X': Unknown types
    deriving (Show, Eq, Lift)

instance Enum PgTypCategory where
    fromEnum :: PgTypCategory -> Int
    fromEnum TcA = 0
    fromEnum TcB = 1
    fromEnum TcC = 2
    fromEnum TcD = 3
    fromEnum TcE = 4
    fromEnum TcG = 5
    fromEnum TcI = 6
    fromEnum TcN = 7
    fromEnum TcP = 8
    fromEnum TcR = 9
    fromEnum TcS = 10
    fromEnum TcT = 11
    fromEnum TcU = 12
    fromEnum TcV = 13
    fromEnum TcX = 14
    toEnum :: Int -> PgTypCategory
    toEnum 0 = TcA
    toEnum 1 = TcB
    toEnum 2 = TcC
    toEnum 3 = TcD
    toEnum 4 = TcE
    toEnum 5 = TcG
    toEnum 6 = TcI
    toEnum 7 = TcN
    toEnum 8 = TcP
    toEnum 9 = TcR
    toEnum 10 = TcS
    toEnum 11 = TcT
    toEnum 12 = TcU
    toEnum 13 = TcV
    toEnum 14 = TcX
    toEnum _ = error "Invalid PgTypeCategory value"

instance Ord PgTypCategory where
    compare :: PgTypCategory -> PgTypCategory -> Ordering
    compare x y = compare (fromEnum x) (fromEnum y)

data IsNullable
    = NonNullable
    | Nullable
    deriving (Show, Eq, Lift)

data PgType = Scalar Identifier PgTypCategory
    deriving (Show, Eq, Lift)

pgTypePretty :: PgType -> String
pgTypePretty = \cases
    (Scalar identifier _) -> identifierPretty identifier

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
    , typesMap :: [(Identifier, PgType, Name)]
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
