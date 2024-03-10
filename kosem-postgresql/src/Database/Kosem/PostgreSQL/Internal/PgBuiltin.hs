{-# LANGUAGE PatternSynonyms #-}

module Database.Kosem.PostgreSQL.Internal.PgBuiltin where

import Database.Kosem.PostgreSQL.Internal.Types

pattern PgBoolean :: PgType
pattern PgBoolean <- Scalar (UnsafeIdentifier "boolean") where
  PgBoolean = Scalar (UnsafeIdentifier "boolean")

pattern PgUnknown :: PgType
pattern PgUnknown <- Scalar (UnsafeIdentifier "unknown") where
  PgUnknown = Scalar (UnsafeIdentifier "unknown")

pattern PgNumeric :: PgType
pattern PgNumeric <- Scalar (UnsafeIdentifier "numeric") where
  PgNumeric = Scalar (UnsafeIdentifier "numeric")

pattern PgText :: PgType
pattern PgText <- Scalar (UnsafeIdentifier "text") where
  PgText = Scalar (UnsafeIdentifier "text")

pattern PgInteger :: PgType
pattern PgInteger <- Scalar (UnsafeIdentifier "integer") where
  PgInteger = Scalar (UnsafeIdentifier "integer")

pattern PgBigint :: PgType
pattern PgBigint <- Scalar (UnsafeIdentifier "bigint") where
  PgBigint = Scalar (UnsafeIdentifier "bigint")
