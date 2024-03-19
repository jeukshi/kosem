{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Database.Kosem.PostgreSQL.Internal.PgBuiltin where

import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Types
import Language.Haskell.TH (Name)

-- * Types

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
    , binaryOperators :: [(Operator, PgType, PgType, PgType)]
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
        , binaryOperators = pgbinaryOperators
        }

-- * Operators

pgbinaryOperators :: [(Operator, PgType, PgType, PgType)]
pgbinaryOperators =
    -- \| Generated by this query:
    -- ```sql
    -- with kosem_types as (
    --     select *
    --       from (values
    --       ('integer'), ('bigint'), ('text'),
    --     (  'boolean'), ('numeric'), ('unkwnown')) as ty
    -- ), gpt_rank (gpt_rank, op) as (
    -- -- We order operators by their usage ranking, courtesy of ChatGPT.
    --     select row_number() over (), *
    --       from (values
    --     ('='), ('<>'), ('<'), ('<='), ('>'), ('>='),
    --     ('+'), ('-'), ('*'), ('/'), ('%'), ('&'),
    --     ('|'), ('#'), ('||'), ('->'), ('->>'), ('#>'),
    --     ('#>>'), ('@>'), ('<@')) as op
    -- ), bin_operators as (
    --     select * from (
    --         select
    --             o.oprname AS operator_name,
    --             pg_catalog.format_type(o.oprleft, NULL) AS left_type,
    --             pg_catalog.format_type(o.oprright, NULL) AS right_type,
    --             pg_catalog.format_type(o.oprresult, NULL) AS result_type
    --           from pg_catalog.pg_operator o
    --           left join pg_catalog.pg_type t ON o.oprresult = t.oid
    --     ) _
    --       left join gpt_rank on op = operator_name
    --      where left_type in (select * from kosem_types)
    --        and right_type in (select * from kosem_types)
    --        and result_type in (select * from kosem_types)
    --      order by
    --      coalesce(gpt_rank, 99), result_type, operator_name, left_type, right_type
    -- )
    -- select '    ' || ', ("' || operator_name || '", '
    --     || 'Pg' || initcap(left_type) || ', '
    --     || 'Pg' || initcap(right_type) || ', '
    --     || 'Pg' || initcap(result_type) || ')'
    --   from bin_operators;
    -- ```
    [ ("=", PgBigint, PgBigint, PgBoolean)
    , ("=", PgBigint, PgInteger, PgBoolean)
    , ("=", PgBoolean, PgBoolean, PgBoolean)
    , ("=", PgInteger, PgBigint, PgBoolean)
    , ("=", PgInteger, PgInteger, PgBoolean)
    , ("=", PgNumeric, PgNumeric, PgBoolean)
    , ("=", PgText, PgText, PgBoolean)
    , ("<>", PgBigint, PgBigint, PgBoolean)
    , ("<>", PgBigint, PgInteger, PgBoolean)
    , ("<>", PgBoolean, PgBoolean, PgBoolean)
    , ("<>", PgInteger, PgBigint, PgBoolean)
    , ("<>", PgInteger, PgInteger, PgBoolean)
    , ("<>", PgNumeric, PgNumeric, PgBoolean)
    , ("<>", PgText, PgText, PgBoolean)
    , ("<", PgBigint, PgBigint, PgBoolean)
    , ("<", PgBigint, PgInteger, PgBoolean)
    , ("<", PgBoolean, PgBoolean, PgBoolean)
    , ("<", PgInteger, PgBigint, PgBoolean)
    , ("<", PgInteger, PgInteger, PgBoolean)
    , ("<", PgNumeric, PgNumeric, PgBoolean)
    , ("<", PgText, PgText, PgBoolean)
    , ("<=", PgBigint, PgBigint, PgBoolean)
    , ("<=", PgBigint, PgInteger, PgBoolean)
    , ("<=", PgBoolean, PgBoolean, PgBoolean)
    , ("<=", PgInteger, PgBigint, PgBoolean)
    , ("<=", PgInteger, PgInteger, PgBoolean)
    , ("<=", PgNumeric, PgNumeric, PgBoolean)
    , ("<=", PgText, PgText, PgBoolean)
    , (">", PgBigint, PgBigint, PgBoolean)
    , (">", PgBigint, PgInteger, PgBoolean)
    , (">", PgBoolean, PgBoolean, PgBoolean)
    , (">", PgInteger, PgBigint, PgBoolean)
    , (">", PgInteger, PgInteger, PgBoolean)
    , (">", PgNumeric, PgNumeric, PgBoolean)
    , (">", PgText, PgText, PgBoolean)
    , (">=", PgBigint, PgBigint, PgBoolean)
    , (">=", PgBigint, PgInteger, PgBoolean)
    , (">=", PgBoolean, PgBoolean, PgBoolean)
    , (">=", PgInteger, PgBigint, PgBoolean)
    , (">=", PgInteger, PgInteger, PgBoolean)
    , (">=", PgNumeric, PgNumeric, PgBoolean)
    , (">=", PgText, PgText, PgBoolean)
    , ("+", PgBigint, PgBigint, PgBigint)
    , ("+", PgBigint, PgInteger, PgBigint)
    , ("+", PgInteger, PgBigint, PgBigint)
    , ("+", PgInteger, PgInteger, PgInteger)
    , ("+", PgNumeric, PgNumeric, PgNumeric)
    , ("-", PgBigint, PgBigint, PgBigint)
    , ("-", PgBigint, PgInteger, PgBigint)
    , ("-", PgInteger, PgBigint, PgBigint)
    , ("-", PgInteger, PgInteger, PgInteger)
    , ("-", PgNumeric, PgNumeric, PgNumeric)
    , ("*", PgBigint, PgBigint, PgBigint)
    , ("*", PgBigint, PgInteger, PgBigint)
    , ("*", PgInteger, PgBigint, PgBigint)
    , ("*", PgInteger, PgInteger, PgInteger)
    , ("*", PgNumeric, PgNumeric, PgNumeric)
    , ("/", PgBigint, PgBigint, PgBigint)
    , ("/", PgBigint, PgInteger, PgBigint)
    , ("/", PgInteger, PgBigint, PgBigint)
    , ("/", PgInteger, PgInteger, PgInteger)
    , ("/", PgNumeric, PgNumeric, PgNumeric)
    , ("%", PgBigint, PgBigint, PgBigint)
    , ("%", PgInteger, PgInteger, PgInteger)
    , ("%", PgNumeric, PgNumeric, PgNumeric)
    , ("&", PgBigint, PgBigint, PgBigint)
    , ("&", PgInteger, PgInteger, PgInteger)
    , ("|", PgBigint, PgBigint, PgBigint)
    , ("|", PgInteger, PgInteger, PgInteger)
    , ("#", PgBigint, PgBigint, PgBigint)
    , ("#", PgInteger, PgInteger, PgInteger)
    , ("||", PgText, PgText, PgText)
    , ("<<", PgBigint, PgInteger, PgBigint)
    , (">>", PgBigint, PgInteger, PgBigint)
    , ("!~", PgText, PgText, PgBoolean)
    , ("!~*", PgText, PgText, PgBoolean)
    , ("!~~", PgText, PgText, PgBoolean)
    , ("!~~*", PgText, PgText, PgBoolean)
    , ("@@", PgText, PgText, PgBoolean)
    , ("^@", PgText, PgText, PgBoolean)
    , ("~", PgText, PgText, PgBoolean)
    , ("~*", PgText, PgText, PgBoolean)
    , ("~<=~", PgText, PgText, PgBoolean)
    , ("~<~", PgText, PgText, PgBoolean)
    , ("~>=~", PgText, PgText, PgBoolean)
    , ("~>~", PgText, PgText, PgBoolean)
    , ("~~", PgText, PgText, PgBoolean)
    , ("~~*", PgText, PgText, PgBoolean)
    , ("<<", PgInteger, PgInteger, PgInteger)
    , (">>", PgInteger, PgInteger, PgInteger)
    , ("^", PgNumeric, PgNumeric, PgNumeric)
    ]
