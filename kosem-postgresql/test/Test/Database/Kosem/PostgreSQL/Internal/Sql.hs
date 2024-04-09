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
        let query =
                [text|
             select :abc::text
              where :cba::text='abc'
        :?ident{and :xyz::text=:?ident::text }
        :?ident{and :xyz::text='xyz'}
                and true
        :?ident{and :xyz::text='xyz'}
                 or true
        :?ident2{and :xyz::text='xyz'} |]
        it "select constants" do
            let commandInfo =
                    case Typechecker.run emptyDatabase query of
                        Left err -> error $ show err
                        Right ci -> ci
            pPrint commandInfo.input
            bar (T.unpack commandInfo.rawCommand) commandInfo.input

            True `shouldBe` True

-- TODO accum as Builder from ByteString
bar :: String -> [Parameter] -> IO [Builder]
bar queryString parameters = runEff $ \io -> do
    (as, ()) <- yieldToList $ \y -> do
        withStateSource $ \source -> do
            currPartS <- newState source (mempty :: Builder)
            skipS <- newState source (0 :: Int)
            paramsLeftS <- newState source parameters
            next <- nextParam paramsLeftS
            currParamS <- newState source next
            resolvedParamsS <- newState source []
            for_ (zip [(0 :: Int) ..] queryString) $ \(ix, char) -> do
                skip <- get skipS
                currParam <- get currParamS
                case (skip, currParam) of
                    (0, Nothing) -> do
                        modify currPartS (<> Builder.charUtf8 char)
                    (0, Just p) ->
                        if unP p.position /= ix
                            then do
                                effIO io (print char)
                                modify currPartS (<> Builder.charUtf8 char)
                            else undefined
                    (n, _) -> modify skipS pred $> ()

    pure as

nextParam
    :: (st :> es)
    => State [Parameter] st
    -> Eff es (Maybe Parameter)
nextParam params =
    get params >>= \case
        [] -> return Nothing
        (p : ps) -> do
            put params ps
            return $ Just p
