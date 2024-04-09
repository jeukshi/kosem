{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Test.Database.Kosem.PostgreSQL.Internal.Sql where

import Bluefin.Eff (Eff, runEff, (:>))
import Bluefin.IO (effIO)
import Bluefin.Jump (jumpTo, withJump)
import Bluefin.State (State, get, modify, put)
import Bluefin.StateSource (newState, withStateSource)
import Bluefin.Stream (yield, yieldToList)
import Control.Monad (when)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as Builder
import Data.Foldable (for_)
import Data.Functor (($>))
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Diagnostics (P (unP))
import Database.Kosem.PostgreSQL.Internal.Sql.Typechecker as Typechecker
import Database.Kosem.PostgreSQL.Internal.Sql.Types (CommandInfo (..), Parameter (..))
import Test.Db (emptyDatabase)
import Test.Hspec
import Test.TH (text)
import Text.Pretty.Simple (pPrint)

spec :: SpecWith ()
spec = do
    describe "selects" $ do
        it "select constants" do
            pendingWith ""
