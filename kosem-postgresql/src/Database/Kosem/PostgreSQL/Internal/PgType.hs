{-# LANGUAGE PatternSynonyms #-}

module Database.Kosem.PostgreSQL.Internal.PgType where

import Database.Kosem.PostgreSQL.Internal.TypCategory qualified as TypCategory
import Database.Kosem.PostgreSQL.Internal.Types

{- Generated with:
select 'pattern ' || pat_name || ' :: PgType' || '\n'
       'pattern ' || pat_name || ' <- ' || adt_name || ' "' || typ_name || '" "' || alt_name || '" TypCategory.' || typcategory || ' ' || pref || '\n'
       '    where\n'
       '        ' || pat_name || ' = ' || adt_name || ' "' || typ_name || '" "' || alt_name || '" TypCategory.' || typcategory || ' ' || pref || '\n'
  from (
select typ_name::text
     , alt_name::text
     , string_agg(initcap(pat_name), '')::text AS pat_name
     , adt_name::text
     , typcategory::text
     , pref
  from (
select oid
     , typname as typ_name
     , format_type(oid, null) as alt_name
     , unnest(string_to_array(trim('"' from format_type(oid, null)), ' ')) as pat_name
     , case when typtype = 'b' then 'Base'
            when typtype = 'p' then 'Pseudo'
            else 'ERROR' end as adt_name
     , typcategory
     , case when typispreferred then 'Prefered' else 'NotPrefered' end pref
 from pg_type where oid in (select toid from kosem_types)
 )_
 group by oid, typ_name, alt_name, adt_name, typcategory, pref
)_2
-}

pattern Boolean :: PgType
pattern Boolean <- Base "bool" "boolean" TypCategory.B Prefered 1
    where
        Boolean = Base "bool" "boolean" TypCategory.B Prefered 1

pattern Bigint :: PgType
pattern Bigint <- Base "int8" "bigint" TypCategory.N NotPrefered 8
    where
        Bigint = Base "int8" "bigint" TypCategory.N NotPrefered 8

pattern Smallint :: PgType
pattern Smallint <- Base "int2" "smallint" TypCategory.N NotPrefered 2
    where
        Smallint = Base "int2" "smallint" TypCategory.N NotPrefered 2

pattern Integer :: PgType
pattern Integer <- Base "int4" "integer" TypCategory.N NotPrefered 4
    where
        Integer = Base "int4" "integer" TypCategory.N NotPrefered 4

pattern Text :: PgType
pattern Text <- Base "text" "text" TypCategory.S Prefered (-1)
    where
        Text = Base "text" "text" TypCategory.S Prefered (-1)

pattern Real :: PgType
pattern Real <- Base "float4" "real" TypCategory.N NotPrefered 4
    where
        Real = Base "float4" "real" TypCategory.N NotPrefered 4

pattern DoublePrecision :: PgType
pattern DoublePrecision <- Base "float8" "double precision" TypCategory.N Prefered 8
    where
        DoublePrecision = Base "float8" "double precision" TypCategory.N Prefered 8

pattern Unknown :: PgType
pattern Unknown <- Pseudo "unknown" "unknown" TypCategory.X NotPrefered (-2)
    where
        Unknown = Pseudo "unknown" "unknown" TypCategory.X NotPrefered (-2)

pattern Character :: PgType
pattern Character <- Base "bpchar" "character" TypCategory.S NotPrefered (-1)
    where
        Character = Base "bpchar" "character" TypCategory.S NotPrefered (-1)

pattern CharacterVarying :: PgType
pattern CharacterVarying <- Base "varchar" "character varying" TypCategory.S NotPrefered (-1)
    where
        CharacterVarying = Base "varchar" "character varying" TypCategory.S NotPrefered (-1)

pattern Numeric :: PgType
pattern Numeric <- Base "numeric" "numeric" TypCategory.N NotPrefered (-1)
    where
        Numeric = Base "numeric" "numeric" TypCategory.N NotPrefered (-1)
