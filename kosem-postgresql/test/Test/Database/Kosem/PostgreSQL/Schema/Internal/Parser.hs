{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Schema.Internal.Parser where

import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Database.Kosem.PostgreSQL.Schema.Internal.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.TH
import Test.Utils

spec :: SpecWith ()
spec = parallel do
    describe "tableItemP" do
        it "does its job" do
            parseOnly tableItemP "sElEcT abc"
                `shouldBe` Right (TableColumn "sElEcT" "abc")

    describe "tableP" do
        it "does its job" do
            parseOnly
                tableP
                [text|
table abc
    col1 text
    col2 text|]
                `shouldBe` Right do
                    Table
                        "abc"
                        [ TableColumn "col1" "text"
                        , TableColumn "col2" "text"
                        ]
    describe "schemaP" do
        it "does its job" do
            parseOnly
                schemaP
                [text|
database MyDatabase
table abc
    col1 text
    col2 text
table abc2
    col1 text
    col2 text
|]
                `shouldBe` Right do
                    Database
                        "MyDatabase"
                        [ Table
                            "abc"
                            [ TableColumn "col1" "text"
                            , TableColumn "col2" "text"
                            ]
                        , Table
                            "abc2"
                            [ TableColumn "col1" "text"
                            , TableColumn "col2" "text"
                            ]
                        ]
