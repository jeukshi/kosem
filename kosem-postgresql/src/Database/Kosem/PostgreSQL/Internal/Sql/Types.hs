module Database.Kosem.PostgreSQL.Internal.Sql.Types where

import Data.ByteString (ByteString)
import Database.Kosem.PostgreSQL.Internal.Types (Identifier, IsNullable, SqlMapping)
import GHC.Exts (Any)
import Language.Haskell.TH.Lift (Lift)
import Language.Haskell.TH.Syntax (Name)
import Data.List.NonEmpty (NonEmpty)

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
    , input :: CommandInput
    , commandByteString :: ByteString
    }
    deriving (Lift)
