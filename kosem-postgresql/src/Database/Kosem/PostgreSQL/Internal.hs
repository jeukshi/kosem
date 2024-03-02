{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal where

import Control.Concurrent (takeMVar)
import Control.Concurrent.MVar (withMVar)
import Data.ByteString (ByteString)
import Data.Maybe (fromJust)
import Data.Vector (Vector)
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import Database.Kosem.PostgreSQL.Internal.Connection
import Database.Kosem.PostgreSQL.Internal.Query
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Schema.Internal.TH
import Database.PostgreSQL.LibPQ qualified as LibPQ
import Database.PostgreSQL.LibPQ qualified as LibPq
import GHC.Exts (Any)
import GHC.Stack.Types (HasCallStack)

execute
    :: forall t
     . (HasCallStack)
    => Connection
    -> Query (Row t)
    -> IO (Vector (Row t))
execute connection query = do
    withMVar (connectionHandle connection) $ \rawConnection -> do
        LibPQ.execParams
            rawConnection
            query.statement
            []
            LibPQ.Binary
            >>= \case
                -- TODO oops
                Nothing -> error "ops"
                Just execResult -> do
                    LibPQ.errorMessage rawConnection >>= \case
                        Just err -> case err of
                            "" -> pure ()
                            _ -> error $ show err
                        Nothing -> pure ()
                    let columnIndexes = map toEnum [0 .. length query.rowParser]
                    numberOfTuples <- LibPQ.ntuples execResult
                    resultVector <-
                        VM.generateM
                            (fromEnum numberOfTuples)
                            (readRow execResult columnIndexes)
                    V.unsafeFreeze resultVector
  where
    readRow :: LibPQ.Result -> [LibPQ.Column] -> Int -> IO (Row t)
    readRow result cols row = do
        xs <- sequence (getCols result (LibPQ.toRow row) cols)
        -- TODO force evaluation
        let !rowData = zipWith ($) query.rowParser xs
        return $ unsafeSwap rowData query.rowProto

    getCols :: LibPQ.Result -> LibPQ.Row -> [LibPQ.Column] -> [IO (Maybe ByteString)]
    getCols result rowNumber = map (LibPQ.getvalue' result rowNumber)

    unsafeSwap :: [Any] -> Row a -> Row a
    unsafeSwap as _ = Row as

-- TODO fixme
unsafeExecute_
    :: (HasCallStack)
    => Connection
    -> ByteString
    -> IO ()
unsafeExecute_ connection statement = do
    withMVar (connectionHandle connection) $ \rawConnection -> do
        LibPQ.execParams
            rawConnection
            statement
            []
            LibPQ.Binary
            >>= \case
                -- TODO oops
                Nothing -> do
                    error "ops"
                Just result -> do
                    mbError <- LibPQ.errorMessage rawConnection
                    case mbError of
                        Just err -> case err of
                            "" -> pure ()
                            _ -> error $ show err
                        Nothing -> pure ()
