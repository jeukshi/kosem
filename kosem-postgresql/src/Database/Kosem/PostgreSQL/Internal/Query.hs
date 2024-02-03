{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Query where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Ast (STerm (Select))
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Parser (selectCore)
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.TH
import GHC.Exts (Any)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Megaparsec qualified as Megaparsec
import Unsafe.Coerce (unsafeCoerce)

-- TODO type param `fetch` (One/Many)
-- TODO type para `database` - database token
data Query result = Query
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
  let numberOfColumns = case ast of
        Select resultColumns _ -> length resultColumns

  let x = show ast
  [e|
      Query
        { statement = s
        , columns = numberOfColumns
        , rowProto = Row [] :: $(genRowT ["field1", "field2"])
        , rowParser = $(genRowParser 2)
        , astS = x
        }
      |]

sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quotePat = error "quasiquoter used in pattern context"
    , quoteType = error "quasiquoter used in type context"
    , quoteDec = error "quasiquoter used in declaration context"
    , quoteExp = genQ
    }
