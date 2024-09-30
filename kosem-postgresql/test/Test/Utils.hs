module Test.Utils where

import Control.Exception (bracket)
import Data.Text (Text)
import Data.Void (Void)
import Database.Kosem.PostgreSQL.Internal.Connection
import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Database.Kosem.PostgreSQL.Internal.Types (Database (Database))
import Test.Hspec.Megaparsec (initialState)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C

withDB =
    bracket
        (connectConnString "postgres://kosem:kosem@127.0.0.1:5432/kosem")
        \conn -> return ()

-- | Helper for parsing without file name.
parseOnly
    :: Parser a
    -> Text
    -> Either (ParseErrorBundle Text Void) a
parseOnly p = parse p ""

{- | Helper for parsing without file name.
Consumes all input.
-}
parseAll
    :: Parser a
    -> Text
    -> Either (ParseErrorBundle Text Void) a
parseAll p = parse (p <* eof) ""

{- | Helper for incremental parsing.
Used by `succeedsLeaving` etc.
-}
parseInc
    :: Parsec Void s a
    -> s
    -> (State s Void, Either (ParseErrorBundle s Void) a)
parseInc p s = runParser' p (initialState s)
