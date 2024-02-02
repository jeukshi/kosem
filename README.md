# :construction: Under Construction :construction:

Kosem is a raw SQL, type-safe, relational mapping library for Haskell.

```haskell
rows <-
    execute
        connection
        [sql|select username, age from user|]
forM_ rows $ \row -> do
    print row.username
    print row.age
```
