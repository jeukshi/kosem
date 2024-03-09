{-# LANGUAGE PatternSynonyms #-}

module Database.Kosem.PostgreSQL.Internal.PgBuiltin where

import Database.Kosem.PostgreSQL.Internal.Types

pattern PgBoolean :: IsNullable -> SqlType
pattern PgBoolean a <- Scalar "boolean" a where
  PgBoolean a = Scalar "boolean" a

pattern PgUnknown :: IsNullable -> SqlType
pattern PgUnknown a <- Scalar "unknown" a where
  PgUnknown a = Scalar "unknown" a

pattern PgNumeric :: IsNullable -> SqlType
pattern PgNumeric a <- Scalar "numeric" a where
  PgNumeric a = Scalar "numeric" a

pattern PgText :: IsNullable -> SqlType
pattern PgText a <- Scalar "text" a where
  PgText a = Scalar "text" a
