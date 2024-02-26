{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

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
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import TH


y :: Int
y = 10

data D = D


-- Example usage
main :: IO ()
main = do
    let x = 10 :: Int
    let y = "abc" :: String

    print $ toSql @Int x

    -- putStrLn $ [tc|z|]
    -- putStrLn $ $(checkType "x")
