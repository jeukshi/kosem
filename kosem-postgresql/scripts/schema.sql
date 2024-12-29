select toid
     , string_agg(initcap(pat_part), '')::text  AS pat_name
  from (
select *
  from (
     select oid as toid
         , trim('"' from format_type(oid, null)) as pat_name
      from pg_type
     where typnamespace = 11
       and typname not like 'pg%'
       and not starts_with(typname, '_')
       and not starts_with(typname, 'reg')
       and typcategory not in ('G','A','I','R', 'U', 'V', 'T', 'Z')
       and typcategory <> 'P' -- TODO add those
       and typcategory <> 'D' -- TODO add those
       and typname not in
            ( 'bytea', 'oid', 'money', 'name', 'record', 'cstring'
            , 'fdw_handler', 'index_am_handler', 'tsm_handler'
            , 'table_am_handler', 'trigger', 'void'
            , 'internal', 'language_handler', 'event_trigger'
            )
     order by typtype, typcategory
)_
cross join lateral unnest(string_to_array(pat_name, ' '))
            with ordinality as a(pat_part, pat_pos)
order by toid, pat_pos
)_2
group by toid
