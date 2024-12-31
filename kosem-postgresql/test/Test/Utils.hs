{-# LANGUAGE DuplicateRecordFields #-}

module Test.Utils where

import Control.Exception (bracket)
import Data.Scientific (Scientific, scientific)
import Data.Text (Text)
import Data.Void (Void)
import Database.Kosem.PostgreSQL.Internal.Connection
import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Database.Kosem.PostgreSQL.Internal.Types (Database (Database))
import Hedgehog (MonadGen)
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec.Megaparsec (initialState)
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C

-- | example record for testing OverloadedRecordDot
data MyRecord a b c d e = MkMyRecord
    { field1 :: a
    , field2 :: b
    , field3 :: MyOtherRecord c d e
    }

data MyOtherRecord a b c = MkMyOtherRecord
    { field1 :: a
    , field2 :: b
    , field3 :: c
    }

withDB =
    bracket
        (connectConnString "postgres://kosem:kosem@127.0.0.1:5432/kosem")
        \conn -> return ()

-- | Helper for parsing without file name.
parseOnly
    :: Parser a
    -> String
    -> Either (ParseErrorBundle String Void) a
parseOnly p = parse p ""

{- | Helper for parsing without file name.
Consumes all input.
-}
parseAll
    :: Parser a
    -> String
    -> Either (ParseErrorBundle String Void) a
parseAll p = parse (p <* eof) ""

{- | Helper for incremental parsing.
Used by `succeedsLeaving` etc.
-}
parseInc
    :: Parsec Void s a
    -> s
    -> (State s Void, Either (ParseErrorBundle s Void) a)
parseInc p s = runParser' p (initialState s)

genScientific :: (MonadGen m) => m Scientific
genScientific = do
    i <- Gen.integral (Range.linear (-2147483648) 2147483647)
    e <- Gen.integral (Range.linear (-100) 100)
    return $ scientific i e
