{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Schema.Internal.Parser where

import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Database.Kosem.PostgreSQL.Internal.PgBuiltin
import Database.Kosem.PostgreSQL.Internal.PgType qualified as PgType
import Database.Kosem.PostgreSQL.Internal.Types
import Database.Kosem.PostgreSQL.Schema.Internal.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.TH
import Test.Utils

spec :: SpecWith ()
spec = parallel do
    describe "tableItemP" do
        it "does its job" do
            pendingWith "TODO separate parsing from typechecking later"
    {- parseOnly (tableItemP defaultDatabaseConfig) "sElEcT abc"
         `shouldBe` Right (Column "sElEcT" PgText Nullable) -}

    describe "tableP" do
        it "does its job" do
            parseOnly
                (tableP defaultDatabaseConfig)
                [text|
table abc
    col1 text
    col2 text|]
                `shouldBe` Right do
                    Table
                        "abc"
                        [ Column "col1" PgType.Text Nullable
                        , Column "col2" PgType.Text Nullable
                        ]
    describe "schemaP" do
        it "does its job" do
            parseOnly
                (schemaP defaultDatabaseConfig)
                [text|
database MyDatabase
table abc
    col1 text
    col2 text
table abc2
    col1 text not null
    col2 text
|]
                `shouldBe` Right do
                    Database
                        "MyDatabase"
                        []
                        []
                        []
                        []
                        [ Table
                            "abc"
                            [ Column "col1" PgType.Text Nullable
                            , Column "col2" PgType.Text Nullable
                            ]
                        , Table
                            "abc2"
                            [ Column "col1" PgType.Text NonNullable
                            , Column "col2" PgType.Text Nullable
                            ]
                        ]
