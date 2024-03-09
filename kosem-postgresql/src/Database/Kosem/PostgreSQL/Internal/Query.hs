{-# LANGUAGE NoDuplicateRecordFields #-}
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
import Database.Kosem.PostgreSQL.Internal.Ast
import Database.Kosem.PostgreSQL.Internal.Env (runProgram)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Parser (selectCore)
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.Row qualified
import Database.Kosem.PostgreSQL.Internal.TH
import Database.Kosem.PostgreSQL.Internal.Type (exprType, typecheck)
import Database.Kosem.PostgreSQL.Internal.Types
import GHC.Exts (Any)
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Text.Megaparsec qualified as Megaparsec
import Unsafe.Coerce (unsafeCoerce)

-- TODO type param `fetch` (One/Many)
-- TODO type para `database` - database token
data Query result = Query
  { statement :: ByteString
  , columnsNumber :: Int
  , rowProto :: result
  , rowParser :: [Maybe ByteString -> Any]
  , params :: [Maybe ByteString]
  , astS :: String
  }

resultFromAst :: STerm SqlType -> Either String [(Identifier, SqlType)]
resultFromAst (Select resultColumns _ _) = do
  let (errors, columns) = partitionEithers $ map columnName (toList resultColumns)
  case errors of
    [] -> Right columns
    (x : xs) -> Left x
 where
  columnName :: AliasedExpr SqlType -> Either String (Identifier, SqlType)
  columnName = \cases
    (WithAlias expr alias _) -> Right (Identifier alias, exprType expr)
    (WithoutAlias (ECol columnname ty)) -> Right (columnname, ty)
    (WithoutAlias (EPgCast (EParam _ name _) _ ty)) -> Right (name, ty)
    -- FIXME error msg
    (WithoutAlias _) -> Left "every result should have an alias"

lookupTypes :: [(Identifier, SqlType)] -> [(PgType, Name)] -> [(Identifier, Name, IsNullable)]
lookupTypes = \cases
  (x : xs) mappings -> fromMapping x mappings : lookupTypes xs mappings
  [] _ -> []
 where
  fromMapping :: (Identifier, SqlType) -> [(PgType, Name)] -> (Identifier, Name, IsNullable)
  fromMapping (label, ty) mappings = case filter (isInMap ty) mappings of
    [] -> error $ "no mapping for type: " <> T.unpack (unPgType $ toPgType ty)
    [(pgType, name)] -> (label, name, isNullable ty)
    (x : xs) ->
      error $ "too many mapping for type: " <> T.unpack (unPgType $ toPgType ty)
  isInMap :: SqlType -> (PgType, Name) -> Bool
  isInMap sqlType (pgType, _) = case sqlType of
    Scalar ty _ -> ty == pgType
  toPgType :: SqlType -> PgType
  toPgType = \cases
    (Scalar ty _) -> ty

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
  let queryToRun = astToRawSql typedAst
  let params =
        map (\(_, l, t) -> (l, t))
          . sortOn (\(n, _, _) -> n)
          . collectAllVariables
          $ typedAst
  -- \$ case Ast._where typedAst of
  -- Nothing -> []
  -- Just (Ast.Where expr) -> Ast.collectVariables expr
  let hsParams = lookupTypes params (typesMap database)
  let x = show ast
  [e|
    Query
      { statement = queryToRun
      , columnsNumber = numberOfColumns
      , rowProto = Row [] :: $(genRowType hsTypes)
      , rowParser = $(genRowParser hsTypes)
      , params = $(genParamsList hsParams)
      , astS = x
      }
    |]
