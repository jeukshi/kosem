{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoDuplicateRecordFields #-}

module Database.Kosem.PostgreSQL.Internal.Query where

import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.List.NonEmpty (toList)
import Data.List.NonEmpty qualified as NonEmpty
import Data.String (IsString (fromString))
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Ast
import Database.Kosem.PostgreSQL.Internal.Diagnostics (CompileError (..), compileError)
import Database.Kosem.PostgreSQL.Internal.Env (runProgram)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Parser (parse)
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.Row qualified
import Database.Kosem.PostgreSQL.Internal.TH
import Database.Kosem.PostgreSQL.Internal.Type (exprType, typecheck, toDiagnosticSpan)
import Database.Kosem.PostgreSQL.Internal.Types
import GHC.Driver.Errors.Types (GhcMessage (..))
import GHC.Exts (Any)
import GHC.Parser.Errors.Types (PsMessage (PsUnknownMessage))
import GHC.Tc.Errors.Types (TcRnMessage (..))
import GHC.Tc.Types (TcM)
import GHC.Tc.Utils.Monad (addErrAt)
import GHC.Types.Error (mkPlainError)
import GHC.Types.SrcLoc (SrcLoc (RealSrcLoc), SrcSpan, mkRealSrcLoc, mkSrcLoc, mkSrcSpan)
import GHC.Utils.Error (noHints)
import GHC.Utils.Outputable (text)
import Language.Haskell.TH (Exp, Loc (loc_filename), Name, Q)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Loc (..), Q (Q), location)
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
  }

resultFromAst :: STerm TypeInfo -> Either CompileError [(Identifier, TypeInfo)]
resultFromAst (Select resultColumns _ _) = do
  let (errors, columns) = partitionEithers $ map columnName (toList resultColumns)
  case errors of
    [] -> Right columns
    (x : xs) -> Left x
 where
  columnName :: AliasedExpr TypeInfo -> Either CompileError (Identifier, TypeInfo)
  columnName = \cases
    (WithAlias expr alias _) -> Right (alias, exprType expr)
    (WithoutAlias (ECol _ columnname ty)) -> Right (columnname, ty)
    (WithoutAlias (EPgCast _ (EParam _ _ name _) _ _ ty)) -> Right (name, ty)
    -- FIXME error msg
    (WithoutAlias expr) -> Left $ ExprWithNoAlias (toDiagnosticSpan expr) "expression does not have an alias"

lookupTypes
  :: [(Identifier, TypeInfo)] -> [(Identifier, PgType, Name)] -> [(Identifier, Name, IsNullable)]
lookupTypes = \cases
  (x : xs) mappings -> fromMapping x mappings : lookupTypes xs mappings
  [] _ -> []
 where
  fromMapping
    :: (Identifier, TypeInfo) -> [(Identifier, PgType, Name)] -> (Identifier, Name, IsNullable)
  fromMapping (label, ty) mappings = case filter (isInMap ty) mappings of
    [] -> error $ "no mapping for type: " <> show ty
    [(_, pgType, name)] -> (label, name, isNullable ty)
    (x : xs) ->
      error $ "too many mapping for type: " <> show ty
  isInMap :: TypeInfo -> (identifier, PgType, Name) -> Bool
  isInMap sqlType (_, pgType, _) = case sqlType of
    TypeInfo ty _ -> ty == pgType
  toPgType :: TypeInfo -> PgType
  toPgType = \cases
    (TypeInfo ty _) -> ty

prepareQuery
  :: Database
  -> Text
  -> Either
      CompileError
      ( ByteString
      , Int
      , [(Identifier, Name, IsNullable)]
      , [(Identifier, Name, IsNullable)]
      )
prepareQuery database input = do
  ast <- parse input
  let numberOfColumns = case ast of
        Select resultColumns _ _ -> length resultColumns
  typedAst <- runProgram database (typecheck ast)
  resultColumns <- resultFromAst typedAst
  let hsTypes = lookupTypes resultColumns (typesMap database)
  let queryToRun = astToRawSql typedAst
  let params =
        map (\(_, l, t) -> (l, t))
          . sortOn (\(n, _, _) -> n)
          . collectAllVariables
          $ typedAst
  let hsParams = lookupTypes params (typesMap database)
  let x = show ast
  return (queryToRun, numberOfColumns, hsTypes, hsParams)

unsafeSql :: Database -> String -> Q Exp
unsafeSql database userInputString = do
  let userInput = T.pack userInputString
  -- let parserResult = parse userInput
  case prepareQuery database userInput of
    Right (queryToRun, numberOfColumns, hsTypes, hsParams) -> do
      [e|
        Query
          { statement = queryToRun
          , columnsNumber = numberOfColumns
          , rowProto = Row [] :: $(genRowType hsTypes)
          , rowParser = $(genRowParser hsTypes)
          , params = $(genParamsList hsParams)
          }
        |]
    Left e -> do
      compileError userInput e
