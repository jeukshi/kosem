{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DataKinds #-}

module Database.Kosem.PostgreSQL.Internal.Query where

import Database.Kosem.PostgreSQL.Internal.FromField
import Language.Haskell.TH
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.TH
import Database.Kosem.PostgreSQL.Internal.Row
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import GHC.Exts (Any)
import Data.ByteString (ByteString)
import Unsafe.Coerce (unsafeCoerce)
import qualified Text.Megaparsec as Megaparsec
import Database.Kosem.PostgreSQL.Internal.Parser (selectCore)
import qualified Data.Text as T
import Database.Kosem.PostgreSQL.Internal.Ast (STerm)

-- TODO type param `fetch` (One/Many)
-- TODO type para `database` - database token
data Query result
  = Query
  { statement :: ByteString
  , columns :: Int
  , rowProto :: result
  , rowParser :: [Maybe ByteString -> Any]
  , astS :: String
  }

{-genR :: String -> Q Exp
genR s = do
  let rec :: $genRowT = Row []
  c <- runQ [e|Row [] :: $genRowT|]
  return c
-}

fffunc :: Text -> IO [Any]
fffunc = undefined

genQ :: String -> Q Exp
genQ s = do
  let parserResult = Megaparsec.parse selectCore "" (T.pack s)
  let ast = case parserResult of
        Left e -> error (Megaparsec.errorBundlePretty e)
        Right ast -> ast
  let x = show ast
  c <- [e|Query { statement = s
                , columns = 2
                , rowProto = Row [] :: $(genRowT ["field1", "field2"])
                , rowParser = $(genRowParser 2)
                , astS = x
                }
         |]
  return c

sql :: QuasiQuoter
sql =
    QuasiQuoter
        { quotePat = error "quasiquoter used in pattern context"
        , quoteType = error "quasiquoter used in type context"
        , quoteDec = error "quasiquoter used in declaration context"
        , quoteExp = genQ
        }
