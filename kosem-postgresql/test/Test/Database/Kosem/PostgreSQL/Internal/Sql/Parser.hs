{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal.Sql.Parser where

import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.ParserUtils
import Database.Kosem.PostgreSQL.Internal.Sql.Parser
import Test.Hspec
import Test.Hspec.Megaparsec
import Test.TH
import Test.Utils

spec :: SpecWith ()
spec = parallel do
    describe "pKeyword" do
        it "is case insensitive" do
            parseOnly (pKeyword "sElEcT")
                `shouldSucceedOn` "SeLeCt"

        it "fails when part of longer string" do
            parseOnly (pKeyword "select")
                `shouldFailOn` "selectt"

        it "can be followed by '('" do
            parseOnly (pKeyword "select")
                `shouldSucceedOn` "select("

        {- it "can't be followed by dot '.'" $ do
          parseOnly (pKeyword "select")
            `shouldFailOn` "select."
        TODO fails^ -}

        it "consumes whitespace after" do
            parseInc (pKeyword "select") "select ("
                `succeedsLeaving` "("

        it "doesn't consume '(' after" do
            parseInc (pKeyword "select") "select("
                `succeedsLeaving` "("

    describe "columnName" do
        it "" do
            parseInc (pKeyword "select") "select("
                `succeedsLeaving` "("

    describe "selectCore" do
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` "select id, name, col123_ from tab1"
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
            select id, name as name, col123_ as col from tab1 join tab2 on true|]
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
            select id from tab1 join tab2 on true join tab3 on true|]
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
            select id from tab1 join tab2 on true join tab3 on true join tab4 on true |]
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
            select id as x
                 , id2 xx
              from tab1 join tab2 on true join tab3 on true join tab4 on true |]
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
            select 'abc' , 'xyz' |]
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
            select false and true alias_works
                 , not false and not true as as_alias_works
                 , not false or not true and false or true
              from tab1
              join tab2 on true and not true or false
              join tab3 on false and not false or true|]
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
            select true
             where true and false
              |]
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
            select true
              from tab1
             where true and false
              |]
        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
            select true > false
                 , true < false
                 , true = false
                 , true != false
                 , true <> false
                 , true <= false
                 , true >= false
             where a = b and c <> d
              |]

        it "" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
             select 'abc'
              where true and true between false and true and true
                 or true and true not between false and true and true
               |]
        it "ppostgresql-style cast" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
             select 'abc' ::text
              where 'abc':: text = 'abc'   ::    text
                and true :: boolean = false
                ::boolean
               |]
        it "simple variable" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
             select :abc
              where :cba and :xyz or :zyx
               |]
        it "guarded AND" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
             select :abc
              where :cba=true = false
        :ident{and :xyz or :ident and x = y}
        :ident{and :xyz }
                and true
        :ident{and :xyz}
                 or true
        :ident{and :xyz }
               |]

        it "simple function" do
            parseOnly selectCore
                `shouldSucceedOn` [text|
             select concat('abc','cba', 'xxx')
               |]
