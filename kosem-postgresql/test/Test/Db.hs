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
    col2 text

|]

-- TODO gen from TH, somehow?
sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quotePat = error "quasiquoter used in pattern context"
    , quoteType = error "quasiquoter used in type context"
    , quoteDec = error "quasiquoter used in declaration context"
    , quoteExp = dbSql
    }


{-
[SigD dbSql (AppT (AppT ArrowT (ConT GHC.Base.String)) (AppT (ConT Language.Haskell.TH.Syntax.Q) (ConT Language.Haskell.TH.Syntax.Exp)))
  ,ValD (VarP dbSql) (NormalB (AppE (VarE Database.Kosem.PostgreSQL.Internal.Query.unsafeSql) (AppE (AppE (ConE Database.Kosem.PostgreSQL.Schema.Internal.Parser.Database) (AppE (AppE (VarE Data.Text.unpackCStringLen#) (LitE (BytesPrimL abc))) (LitE (IntegerL 3)))) (ListE [AppE (AppE (ConE Database.Kosem.PostgreSQL.Schema.Internal.Parser.Table) (AppE (AppE (VarE Data.Text.unpackCStringLen#) (LitE (BytesPrimL xyz))) (LitE (IntegerL 3)))) (ListE [AppE (AppE (ConE Database.Kosem.PostgreSQL.Schema.Internal.Parser.TableColumn) (AppE (ConE Database.Kosem.PostgreSQL.Schema.Internal.Parser.ColumnName) (AppE (AppE (VarE Data.Text.unpackCStringLen#) (LitE (BytesPrimL aaa))) (LitE (IntegerL 3))))) (AppE (ConE Database.Kosem.PostgreSQL.Schema.Internal.Parser.PgType) (AppE (AppE (VarE Data.Text.unpackCStringLen#) (LitE (BytesPrimL text))) (LitE (IntegerL 4))))])])))) []

  ,SigD sql (ConT Language.Haskell.TH.Quote.QuasiQuoter)
  ,ValD (VarP sql) (NormalB (RecConE Language.Haskell.TH.Quote.QuasiQuoter
                             [
                               (Language.Haskell.TH.Quote.quotePat

                               ,AppE (VarE GHC.Err.error) (LitE (StringL "quasiquoter used in pattern context")))
                             ,(Language.Haskell.TH.Quote.quoteType,AppE (VarE GHC.Err.error) (LitE (StringL "quasiquoter used in type context"))),
                              (Language.Haskell.TH.Quote.quoteDec,AppE (VarE GHC.Err.error) (LitE (StringL "quasiquoter used in declaration context")))
                             ,(Language.Haskell.TH.Quote.quoteExp,VarE dbSql)])) []]

-}
