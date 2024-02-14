{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal where

import Data.Vector qualified as V
import Database.Kosem.PostgreSQL.Internal
import Database.Kosem.PostgreSQL.Internal.Connection (close, connectConnString)
import Test.Db qualified as Tdb
import Test.Hspec
import Test.Utils

-- import Test.Utils

spec :: SpecWith ()
spec = around withDB $ do
    describe "" $ do
        it "" $ \conn -> do
            rows <-
                execute
                    conn
                    [Tdb.sql|select 'abc' field1, 'xyz' field2|]
            let row = V.head rows
            row.field1 `shouldBe` "abc"
            row.field2 `shouldBe` "xyz"
        it "simple select" $ \conn -> do
            let _ = [Tdb.sql|select abc, abc2 from tab1|]
            "it compiles" `shouldBe` "it compiles"
        it "simple join" $ \conn -> do
            let _ =
                    [Tdb.sql|
                            select abc
                                 , col2
                              from tab1
                              left join tab2
                                on true
                                |]
            "it compiles" `shouldBe` "it compiles"
