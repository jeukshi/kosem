module Database.Kosem.PostgreSQL.Internal.Connection where

import Control.Concurrent (MVar)
import Control.Concurrent.MVar (newMVar, putMVar)
import Control.Exception (finally)
import Control.Exception.Base (mask)
import Data.ByteString (ByteString)
import Database.PostgreSQL.LibPQ qualified as LibPQ
import GHC.MVar (takeMVar)

data Connection = Connection
    { connectionHandle :: {-# UNPACK #-} !(MVar LibPQ.Connection)
    }

connectConnString :: ByteString -> IO Connection
connectConnString connString = do
    connection <- LibPQ.connectdb connString
    -- TODO check connection
    handle <- newMVar connection
    return $ Connection handle

-- close :: Connection -> IO ()
-- close connection = do
-- rawConnection <- takeMVar $ connectionHandle connection
-- LibPQ.finish rawConnection

close :: Connection -> IO ()
close connection =
    mask $ \restore ->
        do
            conn <- takeMVar $ connectionHandle connection
            restore (LibPQ.finish conn)
            `finally` do
                putMVar (connectionHandle connection) =<< LibPQ.newNullConnection
