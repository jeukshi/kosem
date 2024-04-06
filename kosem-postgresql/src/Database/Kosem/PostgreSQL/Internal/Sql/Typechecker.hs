{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Database.Kosem.PostgreSQL.Internal.Sql.Typechecker where

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.List (sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Traversable (traverse)
import Database.Kosem.PostgreSQL.Internal.Diagnostics (
    CompileError (..),
    DiagnosticSpan (..),
    P,
    combineSpans,
 )
import Database.Kosem.PostgreSQL.Internal.PgBuiltin
import Database.Kosem.PostgreSQL.Internal.Sql.Ast
import Database.Kosem.PostgreSQL.Internal.Sql.Env
import Database.Kosem.PostgreSQL.Internal.Sql.Parser
import Database.Kosem.PostgreSQL.Internal.Sql.Types (
    CommandInfo (..),
    Parameter (..),
    ParameterType (..),
 )
import Database.Kosem.PostgreSQL.Internal.Types
import Database.Kosem.PostgreSQL.Schema.Internal.Parser
import Language.Haskell.TH.Syntax (Name)
import Text.Megaparsec (parseMaybe, parseTest)

resultFromAst
    :: STerm TypeInfo
    -> Either CompileError (NonEmpty SqlMapping)
resultFromAst (Select resultColumns _ _) = do
    traverse resultToSqlMapping resultColumns
  where
    resultToSqlMapping
        :: AliasedExpr TypeInfo
        -> Either CompileError SqlMapping
    resultToSqlMapping = \case
        WithAlias expr alias _ -> do
            let typeInfo = exprType expr
            Right $ SqlMapping alias typeInfo.hsType typeInfo.nullable
        WithoutAlias expr -> do
            let typeInfo = exprType expr
            case typeInfo.identifier of
                Just identifier ->
                    Right $
                        SqlMapping
                            { identifier = identifier
                            , hsType = typeInfo.hsType
                            , nullable = typeInfo.nullable
                            }
                Nothing -> Left $ ExprWithNoAlias expr

run
    :: Database
    -> Text
    -> Either
        CompileError
        CommandInfo
run database input = do
    ast <- parse input
    let numberOfColumns = case ast of
            Select resultColumns _ _ -> length resultColumns
    (typedAst, env) <- runProgram database (typecheck ast)
    hsTypes <- resultFromAst typedAst
    let hsParams =
            map (\p -> (p.identifier, p.hsType, p.nullable))
                . sortOn (.number)
                $ env.params
    let x = show ast
    return $
        CommandInfo
            { input = env.params
            , output = hsTypes
            , rawCommand = input
            }

typecheck :: STerm () -> Tc (STerm TypeInfo)
typecheck = \cases
    (Select res (Just (From fromItem)) whereClause) -> do
        tyFromItem <- tcFromItem fromItem
        tcRes <- tcSelectExpr res
        tyWhereClause <- tcWhereClause whereClause
        return $ Select tcRes (Just (From tyFromItem)) tyWhereClause
    (Select res Nothing whereClause) -> do
        tcRes <- tcSelectExpr res
        tyWhereClause <- tcWhereClause whereClause
        return $ Select tcRes Nothing tyWhereClause

tcWhereClause :: Maybe (Where ()) -> Tc (Maybe (Where TypeInfo))
tcWhereClause = \cases
    Nothing -> return Nothing
    (Just (Where expr)) -> do
        tyExpr <- tcExpr expr
        when ((exprType tyExpr).pgType /= PgBoolean) do
            throwError $ ConditionTypeError tyExpr "WHERE"
        return $ Just $ Where tyExpr

tcSelectExpr
    :: NonEmpty (AliasedExpr ())
    -> Tc (NonEmpty (AliasedExpr TypeInfo))
tcSelectExpr = mapM tc
  where
    tc :: AliasedExpr () -> Tc (AliasedExpr TypeInfo)
    tc = \cases
        (WithAlias expr colAlias maybeAs) -> do
            tcExpr <- tcExpr expr
            return $ WithAlias tcExpr colAlias maybeAs
        (WithoutAlias expr) -> do
            tcExpr <- tcExpr expr
            return $ WithoutAlias tcExpr

tcFromItem :: FromItem () -> Tc (FromItem TypeInfo)
tcFromItem = \cases
    (FiTableName p tableName) -> do
        getTableByName tableName >>= \case
            [] -> throwError $ TableDoesNotExist p tableName
            [table] -> do
                addTableToEnv table
                return $ FiTableName p tableName
            ts -> throwError $ TableNameIsAmbigious p tableName
    (FiJoin lhs joinType rhs joinCondition) -> do
        tyLhs <- tcFromItem lhs
        tyRhs <- tcFromItem rhs
        tyJoinCondition <- tcJoinCondition joinCondition
        return $ FiJoin tyLhs joinType tyRhs tyJoinCondition

tcJoinCondition :: JoinCondition () -> Tc (JoinCondition TypeInfo)
tcJoinCondition = \cases
    JcUsing -> return JcUsing
    (JcOn expr) -> do
        tyExpr <- tcExpr expr
        when ((exprType tyExpr).pgType /= PgBoolean) do
            throwError $ ConditionTypeError tyExpr "JOIN/ON"
        return $ JcOn tyExpr

exprType :: Expr TypeInfo -> TypeInfo
exprType = \cases
    (EPgCast _ _ _ _ ty) -> ty
    (EParens _ _ _ ty) -> ty
    (EParam _ _ _ ty) -> ty
    (EParamMaybe _ _ _ ty) -> ty
    (EGuardedAnd{}) -> TypeInfo PgBoolean Nullable Nothing ''Bool
    (ELit _ _ ty) -> ty
    (ECol _ _ ty) -> ty
    (ENot{}) -> TypeInfo PgBoolean Nullable Nothing ''Bool
    (EAnd{}) -> TypeInfo PgBoolean Nullable Nothing ''Bool
    (EOr{}) -> TypeInfo PgBoolean Nullable Nothing ''Bool
    (EBinOp _ _ _ _ ty) -> ty
    (EBetween{}) -> TypeInfo PgBoolean Nullable Nothing ''Bool
    (ENotBetween{}) -> TypeInfo PgBoolean Nullable Nothing ''Bool

tcNullable :: IsNullable -> IsNullable -> IsNullable
tcNullable = \cases
    NonNullable NonNullable -> NonNullable
    _ _ -> Nullable

tcExpr :: Expr () -> Tc (Expr TypeInfo)
tcExpr = \cases
    (EPgCast p1 var@(EParam pParam _ name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        ty <- getType identifier
        hsType <- getHsType ty
        paramNumber <- introduceParameter name ty hsType SimpleParameter NonNullable
        let typeInfo = TypeInfo ty NonNullable (Just name) hsType
        let tyVar = EParam pParam paramNumber name typeInfo
        return $ EPgCast p1 tyVar p2 identifier typeInfo
    (EPgCast p1 var@(EParamMaybe pParam _ name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        ty <- getType identifier
        hsType <- getHsType ty
        paramNumber <- introduceParameter name ty hsType SimpleMaybeParameter Nullable
        let typeInfo = TypeInfo ty Nullable (Just name) hsType
        let tyVar = EParamMaybe pParam paramNumber name typeInfo
        return $ EPgCast p1 tyVar p2 identifier typeInfo
    (EPgCast p1 expr p2 identifier ()) -> do
        tyExpr <- tcExpr expr
        -- FIXME check text, check if can be casted
        -- FIXME preserve IsNullable from underlying type
        ty <- getType identifier
        hsType <- getHsType ty
        return $
            EPgCast
                p1
                tyExpr
                p2
                identifier
                (TypeInfo ty Nullable (Just identifier) hsType)
    (EParens p1 expr p2 ()) -> do
        tyExpr <- tcExpr expr
        let innerTy = exprType tyExpr
        return $ EParens p1 tyExpr p2 innerTy
    (EParam p _ name ()) -> throwError $ ParameterWithoutCastError p name
    (EParamMaybe p _ name ()) -> throwError $ MaybeParameterWithoutCastError p name
    (ELit p litVal _) -> case litVal of
        NumericLiteral -> do
            hsType <- getHsType PgNumeric
            return $ ELit p litVal (TypeInfo PgNumeric NonNullable Nothing hsType)
        TextLiteral _ -> do
            hsType <- getHsType PgText
            return $ ELit p litVal (TypeInfo PgText NonNullable Nothing hsType) -- FIXME this is 'unknown'
        (BoolLiteral _) -> do
            hsType <- getHsType PgBoolean
            return $ ELit p litVal (TypeInfo PgBoolean NonNullable Nothing hsType)
    (ECol p colName _) -> do
        getColumnByName colName >>= \case
            [] -> throwError $ ColumnDoesNotExist p colName
            [envCol] -> do
                hsType <- getHsType envCol.typeName
                return $
                    ECol
                        p
                        colName
                        (TypeInfo envCol.typeName envCol.nullable (Just envCol.label) hsType)
            ts ->
                throwError $ ColumnNameIsAmbigious p colName
    expr@(ENot p not innerExpr) -> do
        tyExpr <- tcExpr innerExpr
        let ty = exprType tyExpr
        when (ty.pgType /= PgBoolean) do
            throwError $ ConditionTypeError tyExpr "NOT"
        return $ ENot p not tyExpr
    (EGuardedAnd lhs p1 identifier rhs p2) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean "AND" lhs rhs
        return $ EGuardedAnd tyLhs p1 identifier tyRhs p2
    (EAnd p lhs and rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean "AND" lhs rhs
        return $ EAnd p tyLhs and tyRhs
    (EOr p lhs or rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean "OR" lhs rhs
        return $ EOr p tyLhs or tyRhs
    (EBinOp p lhs op rhs ()) -> do
        tcLhs <- tcExpr lhs
        tcRhs <- tcExpr rhs
        let (TypeInfo tyLhs nullableLhs _ _) = exprType tcLhs
        let (TypeInfo tyRhs nullableRhs _ _) = exprType tcRhs
        let nullableRes = tcNullable nullableLhs nullableRhs
        getBinaryOpResult tyLhs op tyRhs >>= \case
            Just tyRes -> do
                hsType <- getHsType tyRes
                return $ EBinOp p tcLhs op tcRhs (TypeInfo tyRes nullableRes Nothing hsType)
            Nothing -> throwError $ OperatorDoesntExist p tyLhs op tyRhs
    (EBetween p lhs between rhs1 and rhs2) -> do
        -- TODO typecheck `between` against <= >=
        tyLhs <- tcExpr lhs
        tyRhs1 <- tcExpr rhs1
        tyRhs2 <- tcExpr rhs2
        EBetween p
            <$> tcExpr lhs
            <*> pure between
            <*> tcExpr rhs1
            <*> pure and
            <*> tcExpr rhs2
    (ENotBetween p lhs not between rhs1 and rhs2) ->
        ENotBetween p
            <$> tcExpr lhs
            <*> pure not
            <*> pure between
            <*> tcExpr rhs1
            <*> pure and
            <*> tcExpr rhs2
  where
    tyMustBeBoolean :: Text -> Expr () -> Expr () -> Tc (Expr TypeInfo, Expr TypeInfo)
    tyMustBeBoolean func lhs rhs = do
        tyLhs <- tcExpr lhs
        tyRhs <- tcExpr rhs
        when ((exprType tyLhs).pgType /= PgBoolean) do
            throwError $ ArgumentTypeError tyLhs func PgBoolean
        when ((exprType tyRhs).pgType /= PgBoolean) do
            throwError $ ArgumentTypeError tyRhs func PgBoolean
        return (tyLhs, tyRhs)

addTableToEnv :: Table -> Tc ()
addTableToEnv table =
    addFieldsToEnv . map (toEnvElem table.name) . columns $ table
  where
    toEnvElem :: Identifier -> Column -> Field
    toEnvElem alias column =
        Field
            { alias = alias
            , label = column.name
            , typeName = column.typeName
            , nullable = column.nullable
            }
