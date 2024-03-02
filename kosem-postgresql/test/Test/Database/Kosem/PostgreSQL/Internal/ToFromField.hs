module Test.Database.Kosem.PostgreSQL.Internal.ToFromField where

import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.FromField (parseField)
import Database.Kosem.PostgreSQL.Internal.ToField (ToField (toField))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (
    PropertyT,
    diff,
    forAll,
    hedgehog,
    (/==),
    (===),
 )

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
