{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal where

import Data.Vector qualified as V
import Database.Kosem.PostgreSQL.Internal
import Database.Kosem.PostgreSQL.Internal.Connection (close, connectConnString)
import Database.Kosem.PostgreSQL.Internal.Query (sql)
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
                    [sql|select 'abc' field1, 'xyz' field2|]
            let row = V.head rows
            row.field1 `shouldBe` "abc"
            row.field2 `shouldBe` "xyz"
        it "" $ \conn -> do
            --rows <-
                --execute
                    --conn
                    --[sql|select 'abc', 'xyz'|]
            -- (rows V.!? 2) `shouldBe` Nothing
            pendingWith "no instance show for Row"
