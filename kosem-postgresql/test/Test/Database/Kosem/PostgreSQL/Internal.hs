{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal where

import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Vector qualified as V
import Database.Kosem.PostgreSQL.Internal
import Database.Kosem.PostgreSQL.Internal.Connection (close, connectConnString)
import Test.Db qualified as Tdb
import Test.Hspec
import Test.TH (text)
import Test.Utils (MyOtherRecord (..), MyRecord (..), withDB)

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
                 where true :guardTrue{and type_text <> :someText::text}
                 |]
            let guardFalse = False
            rowsOne <-
                execute
                    conn
                    [Tdb.sql|
                select type_text
                  from data_types
                 where true :guardFalse{and type_text<>:someText::text}
                 |]
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     delete from data_types
                    |]
            length rowsZero `shouldBe` 0
            length rowsOne `shouldBe` 1

        it "and double guard" $ \conn -> do
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     insert
                       into data_types
                     values ('some text', 9000, 9001, True)
                    |]
            let someText = "some text"
            let guardTrue = True
            let guardTrue2 = True
            rowsZero <-
                execute
                    conn
                    [Tdb.sql|
                select type_text
                  from data_types
                 where true :guardTrue{and type_text <> :someText::text}
                            :guardTrue2{and type_text <> :someText::text}
                 |]
            let guardFalse = False
            let guardFalse2 = False
            rowsOne <-
                execute
                    conn
                    [Tdb.sql|
                select type_text
                  from data_types
                 where true :guardFalse{and type_text<>:someText::text}
                            :guardFalse2{and type_text <> :someText::text}
                 |]
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     delete from data_types
                    |]
            length rowsZero `shouldBe` 0
            length rowsOne `shouldBe` 1

        it "mixed and guards" $ \conn -> do
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     insert
                       into data_types
                     values ('some text', 9000, 9001, True)
                    |]
            let someText = "some text"
            let guardTrue = True
            let guardTrue2 = True
            let mbGuardTrue = Just True
            let mbGuardTrue2 = Just True
            rowsZero <-
                execute
                    conn
                    [Tdb.sql|
                select type_text
                  from data_types
                 where true :guardTrue{and type_text <> :someText::text}
                            and true = true
                            :guardTrue2{and type_text <> :someText::text}
                            and false = false
                            :?mbGuardTrue{and type_text <> :someText::text}
                            :?mbGuardTrue2{and type_text <> :someText::text}
                            :guardTrue2{and type_text <> :someText::text}
                            :guardTrue2{and type_text <> :someText::text}
                            and false = false
                 |]
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     delete from data_types
                    |]
            length rowsZero `shouldBe` 0

        it "and guard (OverloadedRecordDot)" $ \conn -> do
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     insert
                       into data_types
                     values ('some text', 9000, 9001, True)
                    |]
            let myRec =
                    MkMyRecord
                        { field1 = True
                        , field2 = False
                        , field3 =
                            MkMyOtherRecord
                                { field1 = True
                                , field2 = False
                                , field3 = "not important"
                                }
                        }

            let someText = "some text"
            rowsZero <-
                execute
                    conn
                    [Tdb.sql|
                select type_text
                  from data_types
                 where true :myRec.field1{and type_text <> :someText::text}
                            :myRec.field3.field1{and type_text <> :someText::text}
                    |]
            rowsOne <-
                execute
                    conn
                    [Tdb.sql|
                select type_text
                  from data_types
                 where true :myRec.field2{and type_text<>:someText::text}
                            :myRec.field3.field2{and type_text <> :someText::text}
                    |]
            unsafeExecute_ conn do
                T.encodeUtf8
                    [text|
                     delete from data_types
                    |]
            length rowsZero `shouldBe` 0
            length rowsOne `shouldBe` 1

        it "call function" $ \conn -> do
            let abc = True
                cba = "cba"
            rows <- execute conn do
                [Tdb.sql| select length('12345') as len |]
            let row = V.head rows
            row.len `shouldBe` 5

        it "call function (multi arg)" $ \conn -> do
            let someText = "aabb"
            rows <- execute conn do
                [Tdb.sql| select replace(:someText::text, 'a', 'b') as t |]
            let row = V.head rows
            row.t `shouldBe` "bbbb"

        it "OverloadedRecordDot identifier (nested)" $ \conn -> do
            let myRec =
                    MkMyRecord
                        { field1 = "aabb"
                        , field2 = "a"
                        , field3 =
                            MkMyOtherRecord
                                { field1 = "not important"
                                , field2 = "b"
                                , field3 = "not important"
                                }
                        }
            rows <- execute conn do
                [Tdb.sql| select replace(
                          :myRec.field1::text
                        , :myRec.field2::text
                        , :myRec.field3.field2::text) as t |]
            let row = V.head rows
            rows2 <- execute conn do
                [Tdb.sql| select :row.t::text = 'bbbb' as bool |]
            let row2 = V.head rows2
            row2.bool `shouldBe` True
