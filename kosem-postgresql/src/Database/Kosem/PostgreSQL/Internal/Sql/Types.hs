module Database.Kosem.PostgreSQL.Internal.Sql.Types where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Database.Kosem.PostgreSQL.Internal.P (P)
import Database.Kosem.PostgreSQL.Internal.Types (Identifier, IsNullable, PgType, SqlMapping)
import GHC.Exts (Any)
import Language.Haskell.TH.Lift (Lift)
import Language.Haskell.TH.Syntax (Name)
import Data.Text (Text)

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
    , rawCommand :: Text
    }

data ParameterType
    = SimpleParameter
    | SimpleMaybeParameter
    deriving (Show, Eq)

parameterTypeToText :: ParameterType -> Text
parameterTypeToText = \case
  SimpleParameter -> ":"
  SimpleMaybeParameter -> ":?"

data Parameter = Parameter
    { number :: Int
    , identifier :: Identifier
    , pgType :: PgType
    , hsType :: Name
    , paramType :: ParameterType
    , nullable :: IsNullable
    }
    deriving (Show)
