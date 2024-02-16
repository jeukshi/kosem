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
    describe "selects" $ do
        it "select constants" $ \conn -> do
            rows <-
                execute
                    conn
                    [Tdb.sql| select false field1
                                   , true field2
                                   , 'xyz' field3
                                   |]
            let row = V.head rows
            row.field1 `shouldBe` False
            row.field2 `shouldBe` True
            row.field3 `shouldBe` "xyz"
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
