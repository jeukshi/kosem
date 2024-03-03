{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Query where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.List.NonEmpty (toList)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Ast (
  AliasedExpr (..),
  Expr (..),
  STerm (Select),
  SqlType (..),
 )
import Database.Kosem.PostgreSQL.Internal.Ast qualified as Ast
import Database.Kosem.PostgreSQL.Internal.Env (runProgram)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Parser (selectCore)
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.Row qualified
import Database.Kosem.PostgreSQL.Internal.TH
import Database.Kosem.PostgreSQL.Internal.Type (typecheck, exprType)
import Database.Kosem.PostgreSQL.Schema.Internal.Parser (Database (..), PgType, unPgType)
import Database.Kosem.PostgreSQL.Schema.Internal.Parser qualified
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
  , params :: [ByteString]
  , astS :: String
  }

{-genR :: String -> Q Exp
genR s = do
  let rec :: $genRowT = Row []
  c <- runQ [e|Row [] :: $genRowT|]
  return c
-}

resultFromAst :: STerm Ast.SqlType -> Either String [(Text, Ast.SqlType)]
resultFromAst (Select resultColumns _ _) = do
  let (errors, columns) = partitionEithers $ map columnName (toList resultColumns)
  case errors of
    [] -> Right columns
    (x : xs) -> Left x
 where
  columnName :: AliasedExpr Ast.SqlType -> Either String (Text, Ast.SqlType)
  columnName = \cases
    (WithAlias expr alias _) -> Right (alias, exprType expr)
    (WithoutAlias (ECol columnname ty)) -> Right (columnname, ty)
    (WithoutAlias (EPgCast (EVariable _ name _) _ ty)) -> Right (name, ty)
    -- FIXME error msg
    (WithoutAlias _) -> Left "every result should have an alias"

lookupTypes :: [(Text, Ast.SqlType)] -> [(PgType, Name)] -> [(String, Name)]
lookupTypes = \cases
  (x : xs) mappings -> fromMapping x mappings : lookupTypes xs mappings
  [] _ -> []
 where
  fromMapping :: (Text, Ast.SqlType) -> [(PgType, Name)] -> (String, Name)
  fromMapping (label, ty) mappings = case filter (isInMap ty) mappings of
    [] -> error $ "no mapping for type: " <> T.unpack (unPgType $ toPgType ty)
    [(pgType, name)] -> (T.unpack label, name)
    (x : xs) ->
      error $ "too many mapping for type: " <> T.unpack (unPgType $ toPgType ty)
  isInMap :: Ast.SqlType -> (PgType, Name) -> Bool
  isInMap sqlType (pgType, _) = case sqlType of
    Scalar ty -> ty == pgType
    UnknownParam -> error $ "unknown type: " <> show sqlType
  toPgType :: Ast.SqlType -> PgType
  toPgType = \cases
    (Scalar ty) -> ty
    UnknownParam -> error "unknown type"

unsafeSql :: Database -> String -> Q Exp
unsafeSql database userInput = do
  let parserResult = Megaparsec.parse selectCore "" (T.pack userInput)
  let ast = case parserResult of
        Left e -> error (Megaparsec.errorBundlePretty e)
        Right ast -> ast
  let numberOfColumns = case ast of
        Select resultColumns _ _ -> length resultColumns
  let typedAst = case runProgram database (typecheck ast) of
        Left e -> error (show e)
        Right ast -> ast
  let resultColumns = case resultFromAst typedAst of
        Left e -> error (show e)
        Right resultColumns -> resultColumns
  let hsTypes = lookupTypes resultColumns (typesMap database)
  let hsNames = map snd hsTypes
  let queryToRun = Ast.astToRawSql typedAst
  let params = map (\(_, l, t) -> (l, t))
        . sortOn (\(n, _, _) -> n)
        . Ast.collectAllVariables $ typedAst
        -- $ case Ast._where typedAst of
          -- Nothing -> []
          -- Just (Ast.Where expr) -> Ast.collectVariables expr
  let hsParams = lookupTypes params (typesMap database)
  let x = show ast
  [e|
    Query
      { statement = queryToRun
      , columns = numberOfColumns
      , rowProto = Row [] :: $(genRowT hsTypes)
      , rowParser = $(genRowParser hsNames)
      , params = $(genParams hsParams)
      , astS = x
      }
    |]

{- | FIXME remove later on.
Leave it be for now, as an example of how to build QQ.
-}

{- db = Database "" []
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
-}
