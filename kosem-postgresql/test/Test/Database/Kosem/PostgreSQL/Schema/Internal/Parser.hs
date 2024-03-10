{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Schema.Internal.Parser where

import Database.Kosem.PostgreSQL.Internal.ParserUtils
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
            parseOnly tableItemP "sElEcT abc"
                `shouldBe` Right (Column (UnsafeIdentifier "sElEcT") (Scalar (UnsafeIdentifier "abc")))

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
                        (UnsafeIdentifier "abc")
                        [ Column (UnsafeIdentifier "col1") (Scalar (UnsafeIdentifier "text"))
                        , Column (UnsafeIdentifier "col2") (Scalar (UnsafeIdentifier "text"))
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
                        []
                        [ Table
                            (UnsafeIdentifier "abc")
                            [ Column (UnsafeIdentifier "col1") (Scalar (UnsafeIdentifier "text"))
                            , Column (UnsafeIdentifier "col2") (Scalar (UnsafeIdentifier "text"))
                            ]
                        , Table
                            (UnsafeIdentifier "abc2")
                            [ Column (UnsafeIdentifier "col1") (Scalar (UnsafeIdentifier "text"))
                            , Column (UnsafeIdentifier "col2") (Scalar (UnsafeIdentifier "text"))
                            ]
                        ]
