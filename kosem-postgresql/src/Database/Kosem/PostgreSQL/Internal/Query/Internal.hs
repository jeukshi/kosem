{-# LANGUAGE DuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Query.Internal where
import Database.Kosem.PostgreSQL.Internal.Env


-- FIXME remove
schema =
    Schema
        { tables =
            [ Table
                { name = "tab1"
                , columns = [Column "a" "text", Column "b" "text"]
                }
            , Table
                { name = "tab2"
                , columns = [Column "a2" "text", Column "b2" "text"]
                }
            , Table
                { name = "tab3"
                , columns = [Column "a3" "text", Column "b3" "text"]
                }
            ]
        }
