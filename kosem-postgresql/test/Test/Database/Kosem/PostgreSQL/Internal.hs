{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal where

import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Database.Kosem.PostgreSQL.Internal
import Database.Kosem.PostgreSQL.Internal.Connection (close, connectConnString)
import Test.Db qualified as Tdb
import Test.Hspec
import Test.TH (text)
import Test.Utils (withDB)

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
            let hello = True
                ello = False
            let _ =
                    [Tdb.sql|
                            select abc
                                 , col2
                                 , col
                              from tab1
                              left join tab2
                                on abc = col
                             where col = abc2
                               and :hello::boolean
                               and :ello::boolean
                               and col || abc = 'abc'
                                |]
            "it compiles" `shouldBe` "it compiles"
    describe "parameters" do
        it "in output don't need an alias" $ \conn -> do
            let abc = True
            let _ = [Tdb.sql| select :abc::boolean|]
            "it compiles" `shouldBe` "it compiles"

        it "in output can have an alias" $ \conn -> do
            let abc = True
                cba = "cba"
            let _ =
                    [Tdb.sql| select :abc::boolean abc
                                   , :cba::text as cba|]
            "it compiles" `shouldBe` "it compiles"
        it "maybe parameters" $ \conn -> do
            let abc = Just True
                cba = Just "cba"
            let _ =
                    [Tdb.sql| select :?abc::boolean abc
                                   , :?cba::text as cba|]
            "it compiles" `shouldBe` "it compiles"

        it "can be repeated" $ \conn -> do
            let abc = True
                cba = "cba"
            _ <- execute conn do
                [Tdb.sql| select :abc::boolean abc
                               , :cba::text as cba
                               , :abc::boolean as abc2 |]
            "it compiles" `shouldBe` "it compiles"
    describe "selects for real" $ do
        it "selects postgresql simple datatypes" $ \conn -> do
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     insert
                       into data_types
                     values ('some text', 9000, 9001, True)
                    |]
            let param = True
            rows <-
                execute
                    conn
                    [Tdb.sql| select type_text
                                   , type_integer
                                   , type_bigint
                                   , type_boolean
                                from data_types
                               where :param::boolean
                                   |]
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     delete from data_types
                    |]
            let row = V.head rows
            row.type_text `shouldBe` "some text"
            row.type_integer `shouldBe` 9000
            row.type_bigint `shouldBe` 9001
            row.type_boolean `shouldBe` True
    describe "selects with guards" $ do
        it "and guard" $ \conn -> do
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     insert
                       into data_types
                     values ('some text', 9000, 9001, True)
                    |]
            let someText = "some text"
            let guardTrue = True
            rowsZero <-
                execute
                    conn
                    [Tdb.sql|
                select type_text
                  from data_types
                 where true :?guardTrue{and type_text<>:someText::text}
                 |]
            let guardFalse = False
            rowsOne <-
                execute
                    conn
                    [Tdb.sql|
                select type_text
                  from data_types
                 where true :?guardFalse{and type_text<>:someText::text}
                 |]
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     delete from data_types
                    |]
            length rowsZero `shouldBe` 0
            length rowsOne `shouldBe` 1
