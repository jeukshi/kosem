{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.PgBuiltin where

import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Types
import Language.Haskell.TH (Name)

pattern PgBoolean :: PgType
pattern PgBoolean <- Scalar "boolean"
    where
        PgBoolean = Scalar "boolean"

pattern PgUnknown :: PgType
pattern PgUnknown <- Scalar "unknown"
    where
        PgUnknown = Scalar "unknown"

pattern PgNumeric :: PgType
pattern PgNumeric <- Scalar "numeric"
    where
        PgNumeric = Scalar "numeric"

pattern PgText :: PgType
pattern PgText <- Scalar "text"
    where
        PgText = Scalar "text"

pattern PgInteger :: PgType
pattern PgInteger <- Scalar "integer"
    where
        PgInteger = Scalar "integer"

pattern PgBigint :: PgType
pattern PgBigint <- Scalar "bigint"
    where
        PgBigint = Scalar "bigint"

data DatabaseConfig = DatabaseConfig
    { types :: [(Identifier, PgType, Name)]
    }

defaultDatabaseConfig :: DatabaseConfig
defaultDatabaseConfig =
    DatabaseConfig
        { types =
            [ ("text", PgText, ''Text)
            , ("integer", PgInteger, ''Int)
            , ("bigint", PgBigint, ''Int)
            , ("boolean", PgBoolean, ''Bool)
            ]
        }
