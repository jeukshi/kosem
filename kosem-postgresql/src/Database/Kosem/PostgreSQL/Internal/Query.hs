{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Query where

import Data.ByteString (ByteString)
import Data.List.NonEmpty (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Ast (AliasedExpr (..), Expr (..), STerm (Select))
import Database.Kosem.PostgreSQL.Internal.Ast qualified as Ast
import Database.Kosem.PostgreSQL.Internal.Env (runProgram)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Parser (selectCore)
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.TH
import Database.Kosem.PostgreSQL.Internal.Type (typecheck)
import GHC.Exts (Any)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Megaparsec qualified as Megaparsec
import Unsafe.Coerce (unsafeCoerce)
import Data.Either (partitionEithers)
import Database.Kosem.PostgreSQL.Schema.Internal.Parser (Database(..))
import qualified Database.Kosem.PostgreSQL.Internal.Row

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

resultFromAst :: STerm Ast.SqlType -> Either String [Text]
resultFromAst (Select resultColumns _) = do
  let (errors, columns) = partitionEithers $ map columnName (toList resultColumns)
  case errors of
    [] -> Right columns
    (x:xs) -> Left x

 where
  columnName :: AliasedExpr Ast.SqlType -> Either String Text
  columnName = \cases
    (WithAlias _ alias _) -> Right alias
    (WithoutAlias (ECol columnname _)) -> Right columnname
    -- FIXME error msg
    (WithoutAlias _) -> Left "every result should have an alias"

unsafeSql :: Database -> String -> Q Exp
unsafeSql database userInput = do
  let parserResult = Megaparsec.parse selectCore "" (T.pack userInput)
  let ast = case parserResult of
        Left e -> error (Megaparsec.errorBundlePretty e)
        Right ast -> ast
  let numberOfColumns = case ast of
        Select resultColumns _ -> length resultColumns
  let typedAst = case runProgram database (typecheck ast) of
        Left e -> error (show e)
        Right ast -> ast
  let resultColumns = case resultFromAst typedAst of
        Left e -> error (show e)
        Right resultColumns -> map T.unpack resultColumns
  let x = show ast
  [e|
    Query
      { statement = userInput
      , columns = numberOfColumns
      , rowProto = Row [] :: $(genRowT resultColumns)
      , rowParser = $(genRowParser numberOfColumns)
      , astS = x
      }
    |]


-- | FIXME remove later on.
-- Leave it be for now, as an example of how to build QQ.
db = Database "" []
unsafeSql' :: String -> Q Exp
unsafeSql' = unsafeSql db
sql :: QuasiQuoter
sql =
  QuasiQuoter
    { quotePat = error "quasiquoter used in pattern context"
    , quoteType = error "quasiquoter used in type context"
    , quoteDec = error "quasiquoter used in declaration context"
    , quoteExp = unsafeSql'
    }
