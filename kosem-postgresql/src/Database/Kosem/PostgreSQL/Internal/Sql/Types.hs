{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoDuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Sql.Types where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.P (P)
import Database.Kosem.PostgreSQL.Internal.Types (Identifier, IsNullable, PgType, SqlMapping)
import GHC.Exts (Any)
import Language.Haskell.TH.Lift (Lift)
import Language.Haskell.TH.Syntax (Name)

-- TODO type param `fetch` (One/Many)
-- TODO type para `database` - database token
data SqlCommand result = SqlCommand
    { statement :: ByteString
    , columnsNumber :: Int
    , rowProto :: result
    , rowParser :: [Maybe ByteString -> Any]
    , params :: [Maybe ByteString]
    }

-- type CommandInput = [(Identifier, Name, IsNullable)]

data CommandInfo = CommandInfo
    { output :: NonEmpty SqlMapping
    , input :: [CommandInput]
    , rawCommand :: String
    }
    deriving (Show)

parameterTypeToText :: ParameterType -> Text
parameterTypeToText = \case
    SimpleParameter -> ":"
    SimpleMaybeParameter -> ":?"

guardTypeToText :: GuardType -> Text
guardTypeToText = \case
    BooleanGuard -> ":"
    MaybeGuard -> ":?"

data CommandInput
    = CommandParameter Parameter
    | CommandGuard Guard
    deriving (Show)

commandInputPosition :: CommandInput -> P
commandInputPosition = \cases
    (CommandParameter p) -> p.position
    (CommandGuard g) -> g.guardPos

data Guard = Guard
    { guardPos :: P
    , openBracketPos :: P
    , closeBracketPos :: P
    , gIdentifier :: Identifier
    , guardType :: GuardType
    }
    deriving (Show)

data GuardType
    = BooleanGuard
    | MaybeGuard
    deriving (Show, Eq)

data Parameter = Parameter
    { position :: P
    , pIdentifier :: Identifier
    , paramType :: ParameterType
    , info :: Maybe ParameterInfo
    }
    deriving (Show)

data ParameterType
    = SimpleParameter
    | SimpleMaybeParameter
    deriving (Show, Eq)

data ParameterInfo = ParameterInfo
    { pgType :: PgType
    , hsType :: Name
    , nullable :: IsNullable
    }
    deriving (Show)

data Path = Path
    { pathIdentifier :: Identifier
    , pathOption :: PathOption
    }
    deriving (Show)

data PathOption
    = PoFalse
    | PoTrue
    | PoJust
    | PoNothing
    deriving (Show)

x = do
    let x = True
    let y = Just 0
    let z = True
    case (y, x) of
        (Just _, False) -> undefined
        (Just _, True) -> undefined
        (Nothing, True) -> undefined
        (Nothing, False) -> undefined
