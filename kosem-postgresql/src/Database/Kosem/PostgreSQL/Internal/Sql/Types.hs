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

type CommandInput = [(Identifier, Name, IsNullable)]

data CommandInfo = CommandInfo
    { output :: NonEmpty SqlMapping
    , input :: [Parameter]
    , rawCommand :: String
    }

data ParameterType
    = SimpleParameter
    | SimpleMaybeParameter
    | GuardParameter
    deriving (Show, Eq)

parameterTypeToText :: ParameterType -> Text
parameterTypeToText = \case
    SimpleParameter -> ":"
    SimpleMaybeParameter -> ":?"
    GuardParameter -> ":?"

data Parameter = Parameter
    { position :: P
    , identifier :: Identifier
    , paramType :: ParameterType
    , info :: Maybe ParameterInfo
    }
    deriving (Show)

data ParameterInfo = ParameterInfo
    { pgType :: PgType
    , hsType :: Name
    , nullable :: IsNullable
    }
    deriving (Show)
