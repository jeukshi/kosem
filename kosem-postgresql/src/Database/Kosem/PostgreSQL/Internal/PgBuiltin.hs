{-# LANGUAGE PatternSynonyms #-}

module Database.Kosem.PostgreSQL.Internal.PgBuiltin where

import Database.Kosem.PostgreSQL.Internal.Types

pattern PgBoolean :: PgType
pattern PgBoolean <- Scalar "boolean" where
  PgBoolean = Scalar "boolean"

pattern PgUnknown :: PgType
pattern PgUnknown <- Scalar "unknown" where
  PgUnknown = Scalar "unknown"

pattern PgNumeric :: PgType
pattern PgNumeric <- Scalar "numeric" where
  PgNumeric = Scalar "numeric"

pattern PgText :: PgType
pattern PgText <- Scalar "text" where
  PgText = Scalar "text"

pattern PgInteger :: PgType
pattern PgInteger <- Scalar "integer" where
  PgInteger = Scalar "integer"

pattern PgBigint :: PgType
pattern PgBigint <- Scalar "bigint" where
  PgBigint = Scalar "bigint"
