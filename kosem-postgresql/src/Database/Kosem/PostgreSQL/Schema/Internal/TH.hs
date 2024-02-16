{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Schema.Internal.TH where

import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Query
import Database.Kosem.PostgreSQL.Schema.Internal.Parser (Database, schemaP, typesMap)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax (NameSpace (VarName))
import Text.Megaparsec qualified as Megaparsec

database :: QuasiQuoter
database =
    QuasiQuoter
        { quotePat = error "quasiquoter used in pattern context"
        , quoteType = error "quasiquoter used in type context"
        , quoteDec = database'
        , quoteExp = error "quasiquoter used in expression context"
        }

database' :: String -> Q [Dec]
database' userInput = do
    let parserResult = Megaparsec.parse schemaP "" (T.pack userInput)
    let db = case parserResult of
            Left e -> error (Megaparsec.errorBundlePretty e)
            Right db -> db
    let dbWithTypes =
            db
                { typesMap =
                    [ ("text", ''Text)
                    , ("integer", ''Int)
                    , ("bigint", ''Int)
                    , ("boolean", ''Bool)
                    ]
                }
    dbExp <- [e|dbWithTypes|]
    let sql = mkName "sql"
    let dbSql = mkName "dbSql"
    let myDb = applyDatabaseToUnsafeSql dbSql dbExp
    return $ myDb -- ++ xxx sql dbSql

-- db :: Name -> Database -> Q Dec
-- db n dd = [|| n = dd ||]

applyDatabaseToUnsafeSql :: Name -> Exp -> [Dec]
applyDatabaseToUnsafeSql name db =
    [ SigD
        name
        ( AppT
            (AppT ArrowT (ConT ''String))
            (AppT (ConT ''Q) (ConT ''Exp))
        )
    , ValD
        (VarP name)
        ( NormalB
            ( AppE
                (VarE 'unsafeSql)
                db
            )
        )
        []
    ]

-- this doesn't work
-- Type constructor ‘Language.Haskell.TH.Quote.QuasiQuoter’ used as a constructor-like thing
genQuasiQuote :: Name -> Name -> [Dec]
genQuasiQuote sql dbSql =
    [ SigD sql (ConT ''QuasiQuoter)
    , ValD
        (VarP sql)
        ( NormalB
            ( RecConE
                ''QuasiQuoter
                [
                    ( 'quotePat
                    , AppE (VarE 'error) (LitE (StringL "quasiquoter used in pattern context"))
                    )
                ,
                    ( 'quoteType
                    , AppE (VarE 'error) (LitE (StringL "quasiquoter used in type context"))
                    )
                ,
                    ( 'quoteDec
                    , AppE (VarE 'error) (LitE (StringL "quasiquoter used in declaration context"))
                    )
                , ('quoteExp, VarE dbSql)
                ]
            )
        )
        []
    ]
