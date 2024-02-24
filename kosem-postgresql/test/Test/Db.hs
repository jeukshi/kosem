{-# LANGUAGE QuasiQuotes #-}

module Test.Db where

import Database.Kosem.PostgreSQL.Schema.Internal.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))

[database|
database testDb
table tab1
    abc text
    abc2 text

table tab2
    col text
    col2 integer

table data_types
    type_text text
    type_integer integer
    type_bigint bigint
    type_boolean boolean
|]
