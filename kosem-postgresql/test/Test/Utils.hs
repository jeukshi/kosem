module Test.Utils where

import Control.Exception (bracket)
import Database.Kosem.PostgreSQL.Internal.Connection

withDB =
    bracket
        (connectConnString "postgres://kosem:kosem@127.0.0.1:5432/kosem")
        \conn -> return ()
