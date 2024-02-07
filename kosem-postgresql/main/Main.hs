{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString (ByteString)
import Data.Int (Int32)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.Vector qualified as V
import Database.Kosem.PostgreSQL.Internal
import Database.Kosem.PostgreSQL.Internal.Connection
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Query
import Database.Kosem.PostgreSQL.Internal.Row
import Database.PostgreSQL.LibPQ qualified as LibPQ
import Foreign.C (CInt)
import GHC.Exts (Any)
import PostgreSQL.Binary.Decoding
import Unsafe.Coerce (unsafeCoerce)

main :: IO ()
main = do
  example1
  pure ()

example1 :: IO ()
example1 = do
  connection <-
    connectConnString "postgres://kosem:kosem@127.0.0.1:5432/kosem"
  rows <-
    execute
      connection
      [sql|select 'abc' field1, 'xyz' field2|]
  V.forM_ rows $ \row -> do
    print row.field1
    print row.field2
