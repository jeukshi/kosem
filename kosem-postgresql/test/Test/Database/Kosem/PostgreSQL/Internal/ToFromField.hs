{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal.ToFromField where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Vector qualified as V
import Database.Kosem.PostgreSQL.Internal
import Database.Kosem.PostgreSQL.Internal.FromField
    ( parseField, FromField(..) )
import Database.Kosem.PostgreSQL.Internal.ToField
    ( ToField(toField), ToField(..) )
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Db qualified as Tdb
import Test.Hspec
import Test.Hspec.Hedgehog (
    PropertyT,
    diff,
    evalIO,
    forAll,
    hedgehog,
    (/==),
    (===),
 )
import Test.Utils (withDB)

spec :: SpecWith ()
spec = do
    describe "'Bool' instances" do
        it "work for 'True'" do
            (parseField . toField $ True) `shouldBe` True

        it "work for 'False'" do
            (parseField . toField $ False) `shouldBe` False

    describe "'Text' instance" do
        it "parseField . Just . toField == id" $ hedgehog do
            text <- forAll $ Gen.text (Range.constant 0 256) Gen.unicode
            (parseField . toField $ text) === text

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
            (parseField . toField $ int) === int

    describe "'Maybe a' instance" do
        it "work for 'Just a'" do
            (parseField'Internal . toField'Internal $ Just True) `shouldBe` True
            (parseField'Internal . toField'Internal $ Just False) `shouldBe` False

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

    describe "'Text' instances" do
        it "select 'Text'" $ \conn -> hedgehog do
            hsText <- forAll $ Gen.text (Range.constant 0 256) Gen.unicode
            rows <- evalIO do
                execute
                    conn
                    [Tdb.sql| select :hsText::text dbText
                            |]
            let row = V.head rows
            row.dbText === hsText

        it "select 'Text' with '\\NUL' character" $ \conn -> do
            let (hsNullText :: Text) = "\NUL"
            rows <-
                execute
                    conn
                    [Tdb.sql| select :hsNullText::text dbNullText
                            |]
            let row = V.head rows
            -- row.dbNullText `shouldBe` hsNullText
            pendingWith "doesn't work as well"

    describe "'Int' instances" do
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

    describe "'Maybe' instances" do
        it "select 'Maybe Bool'" $ \conn -> do
            let hsMbFalse = Just False
            let hsMbTrue = Just True
            let hsMbTextNothing = Nothing
            let hsMbBoolNothing = Nothing
            let hsMbIntNothing = Nothing
            let query = [Tdb.sql|
                select :?hsMbFalse::boolean dbMbFalse
                     , :?hsMbTrue::boolean dbMbTrue
                     , :?hsMbTextNothing::text dbMbTextNothing
                     , :?hsMbBoolNothing::boolean dbMbBoolNothing
                     , :?hsMbIntNothing::integer dbMbIntNothing
                     |]
            row <- V.head <$> execute conn query
            row.dbMbFalse `shouldBe` hsMbFalse
            row.dbMbTrue `shouldBe` hsMbTrue
            row.dbMbTextNothing `shouldBe` hsMbTextNothing
            row.dbMbBoolNothing `shouldBe` hsMbBoolNothing
            row.dbMbIntNothing `shouldBe` hsMbIntNothing
