{-# LANGUAGE QuasiQuotes #-}

module Test.Db where

import Database.Kosem.PostgreSQL.Schema.Internal.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

[database|
database testDb
table tab1
    abc text not null
    abc2 text

table tab2
    col text
    col2 integer not null

table data_types
    type_text text not null
    type_integer integer not null
    type_bigint bigint not null
    type_boolean boolean not null
|]
