{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal.Parser where

import Test.Hspec.Megaparsec

import Data.Text (Text)
import Data.Void (Void)
import Database.Kosem.PostgreSQL.Internal.Parser
import Test.Hspec
import Test.TH
import Text.Megaparsec
import Text.Megaparsec.Char qualified as C

-- | Helper for parsing without file name.
parseOnly
  :: Parser a
  -> Text
  -> Either (ParseErrorBundle Text Void) a
parseOnly p = parse p ""

{- | Helper for parsing without file name.
Consumes all input.
-}
parseAll
  :: Parser a
  -> Text
  -> Either (ParseErrorBundle Text Void) a
parseAll p = parse (p <* eof) ""

{- | Helper for incremental parsing.
Used by `succeedsLeaving` etc.
-}
parseInc
  :: Parsec Void s a
  -> s
  -> (State s Void, Either (ParseErrorBundle s Void) a)
parseInc p s = runParser' p (initialState s)

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
        `shouldSucceedOn` [sql|
            select id, name as name, col123_ as col from tab1 join tab2 on true|]
    it "" do
      parseOnly selectCore
        `shouldSucceedOn` [sql|
            select id from tab1 join tab2 on true join tab3 on true|]
    it "" do
      parseOnly selectCore
        `shouldSucceedOn` [sql|
            select id from tab1 join tab2 on true join tab3 on true join tab4 on true |]
    it "" do
      parseOnly selectCore
        `shouldSucceedOn` [sql|
            select id as x
                 , id2 xx
              from tab1 join tab2 on true join tab3 on true join tab4 on true |]
    it "" do
      parseOnly selectCore
        `shouldSucceedOn` [sql|
            select 'abc' , 'xyz' |]
