{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal.ToFromField where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Vector qualified as V
import Database.Kosem.PostgreSQL.Internal
import Database.Kosem.PostgreSQL.Internal.FromField (parseField)
import Database.Kosem.PostgreSQL.Internal.ToField (ToField (toField))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Db qualified as Tdb
import Test.Hspec
import Test.Hspec.Hedgehog (
    PropertyT,
    diff,
    forAll,
    hedgehog,
    (/==),
    (===), evalIO,
 )
import Test.Utils (withDB)

spec :: SpecWith ()
spec = do
    describe "'Bool' instances" do
        it "work for 'True'" do
            (parseField . Just . toField $ True) `shouldBe` True

        it "work for 'False'" do
            (parseField . Just . toField $ False) `shouldBe` False

    describe "'Text' instance" do
        it "parseField . Just . toField == id" $ hedgehog do
            text <- forAll $ Gen.text (Range.constant 0 256) Gen.unicode
            (parseField . Just . toField $ text) === text

        it "handles '\\NUL' character" do
            let (text :: Text) = "\NUL"
            -- FIXME doesn't work:
            -- (parseField . Just . toField $ text) `shouldBe` text
            -- SELECT E'\u0000' -- Invalid unicode in PG
            pendingWith "doesn't work"

    describe "'Int' instance" do
        it "parseField . Just . toField == id" $ hedgehog do
            (int :: Int) <- forAll do
                Gen.integral (Range.linear (minBound @Int) (maxBound @Int))
            (parseField . Just . toField $ int) === int

    describe "'Maybe a' instance" do
        it "work for 'Just a'" do
            (parseField . Just . toField $ Just True) `shouldBe` True
            (parseField . Just . toField $ Just False) `shouldBe` False

        it "work for 'Nothing'" do
            let (nothing :: Maybe Bool) = Nothing
            -- (parseField . Just . toField $ nothing) `shouldBe` nothing
            pendingWith "this sadly loops forever"

specIO :: SpecWith ()
specIO = around withDB do
    describe "'Bool' instances" do
        it "select 'Bool'" $ \conn -> do
            let hsFalse = False
            let hsTrue = True
            rows <-
                execute
                    conn
                    [Tdb.sql| select :hsFalse::boolean dbFalse
                                   , :hsTrue::boolean dbTrue
                            |]
            let row = V.head rows
            row.dbFalse `shouldBe` hsFalse
            row.dbTrue `shouldBe` hsTrue
        it "select 'Text'" $ \conn -> hedgehog do
            hsText <- forAll $ Gen.text (Range.constant 0 256) Gen.unicode
            rows <- evalIO do
                execute
                    conn
                    [Tdb.sql| select :hsText::text dbText
                            |]
            let row = V.head rows
            row.dbText === hsText
        it "select 'Int' as PG 'bigint'" $ \conn -> hedgehog do
            (hsInt :: Int) <- forAll do
                Gen.integral (Range.linear (minBound @Int) (maxBound @Int))

            rows <- evalIO do
                execute
                    conn
                    [Tdb.sql| select :hsInt::bigint dbInt
                            |]
            let row = V.head rows
            row.dbInt === hsInt
        it "select 'Int' as PG 'integer'" $ \conn -> do
             pendingWith "handle out of range integers"
