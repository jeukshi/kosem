{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Database.Kosem.PostgreSQL.Internal.Sql.Typechecker where

import Bluefin.Eff
import Bluefin.Exception
import Bluefin.State
import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.Either (partitionEithers)
import Data.List (sortBy, sortOn)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (fromJust, fromMaybe)
import Data.Ord (comparing)
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
    ParameterInfo (..),
    ParameterType (..),
 )
import Database.Kosem.PostgreSQL.Internal.Types
import Database.Kosem.PostgreSQL.Schema.Internal.Parser
import Language.Haskell.TH.Syntax (Name)
import Text.Megaparsec (parseMaybe, parseTest)

resultFromAst
    :: (e :> es)
    => EnvE e
    -> STerm TypeInfo
    -> Eff es (NonEmpty SqlMapping)
resultFromAst env = \cases
    (Select resultColumns _ _) ->
        traverse (resultToSqlMapping env) resultColumns
  where
    resultToSqlMapping
        :: (e :> es)
        => EnvE e
        -> AliasedExpr TypeInfo
        -> Eff es SqlMapping
    resultToSqlMapping env = \case
        WithAlias expr alias _ -> do
            let typeInfo = exprType expr
            return $ SqlMapping alias typeInfo.hsType typeInfo.nullable
        WithoutAlias expr -> do
            let typeInfo = exprType expr
            case typeInfo.identifier of
                Just identifier ->
                    return $
                        SqlMapping
                            { identifier = identifier
                            , hsType = typeInfo.hsType
                            , nullable = typeInfo.nullable
                            }
                Nothing -> throw env.compileError $ ExprWithNoAlias expr

run
    :: Database
    -> String
    -> Either
        CompileError
        CommandInfo
run database input = do
    ast <- parse input
    let numberOfColumns = case ast of
            Select resultColumns _ _ -> length resultColumns
    runPureEff do
        try \ex -> do
            runEnv database ex [] [] \(env :: EnvE e) -> do
                typedAst <- typecheck env ast
                fields <- get @e env.fields
                parameters <- get @e env.parameters
                hsTypes <- resultFromAst env typedAst
                let paramsSorted = sortBy (comparing (.position)) parameters
                return $
                    CommandInfo
                        { input = paramsSorted
                        , output = hsTypes
                        , rawCommand = input
                        }

typecheck
    :: (e :> es) => EnvE e -> STerm () -> Eff es (STerm TypeInfo)
typecheck env = \cases
    (Select res (Just (From fromItem)) whereClause) -> do
        tyFromItem <- tcFromItem env fromItem
        tcRes <- tcSelectExpr env res
        tyWhereClause <- tcWhereClause env whereClause
        return $ Select tcRes (Just (From tyFromItem)) tyWhereClause
    (Select res Nothing whereClause) -> do
        tcRes <- tcSelectExpr env res
        tyWhereClause <- tcWhereClause env whereClause
        return $ Select tcRes Nothing tyWhereClause

tcWhereClause
    :: (e :> es)
    => EnvE e
    -> Maybe (Where ())
    -> Eff es (Maybe (Where TypeInfo))
tcWhereClause env = \cases
    Nothing -> return Nothing
    (Just (Where expr)) -> do
        tyExpr <- tcExpr env expr
        when ((exprType tyExpr).pgType /= PgBoolean) do
            throw env.compileError $ ConditionTypeError tyExpr "WHERE"
        return $ Just $ Where tyExpr

tcSelectExpr
    :: (e :> es)
    => EnvE e
    -> NonEmpty (AliasedExpr ())
    -> Eff es (NonEmpty (AliasedExpr TypeInfo))
tcSelectExpr env = mapM (tc env)
  where
    tc
        :: (e :> es)
        => EnvE e
        -> AliasedExpr ()
        -> Eff es (AliasedExpr TypeInfo)
    tc env = \cases
        (WithAlias expr colAlias maybeAs) -> do
            tcExpr <- tcExpr env expr
            return $ WithAlias tcExpr colAlias maybeAs
        (WithoutAlias expr) -> do
            tcExpr <- tcExpr env expr
            return $ WithoutAlias tcExpr

tcFromItem
    :: (e :> es)
    => EnvE e
    -> FromItem ()
    -> Eff es (FromItem TypeInfo)
tcFromItem env = \cases
    (FiTableName p tableName) -> do
        fiTable <- case getTableByName env.database tableName of
            [] -> throw env.compileError $ TableDoesNotExist p tableName
            [table] -> do
                addTableToEnv env.fields table
                return $ FiTableName p tableName
            ts -> throw env.compileError $ TableNameIsAmbigious p tableName
        return fiTable
    (FiJoin lhs joinType rhs joinCondition) -> do
        tyLhs <- tcFromItem env lhs
        tyRhs <- tcFromItem env rhs
        tyJoinCondition <- tcJoinCondition env joinCondition
        return $ FiJoin tyLhs joinType tyRhs tyJoinCondition

tcJoinCondition
    :: (e :> es)
    => EnvE e
    -> JoinCondition ()
    -> Eff es (JoinCondition TypeInfo)
tcJoinCondition env = \cases
    JcUsing -> return JcUsing
    (JcOn expr) -> do
        tyExpr <- tcExpr env expr
        when ((exprType tyExpr).pgType /= PgBoolean) do
            throw env.compileError $ ConditionTypeError tyExpr "JOIN/ON"
        return $ JcOn tyExpr

exprType :: Expr TypeInfo -> TypeInfo
exprType = \cases
    (EPgCast _ _ _ _ ty) -> ty
    (EParens _ _ _ ty) -> ty
    (EParam _ _ ty) -> ty
    (EParamMaybe _ _ ty) -> ty
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

tcExpr
    :: (e :> es)
    => EnvE e
    -> Expr ()
    -> Eff es (Expr TypeInfo)
tcExpr env = \cases
    (EPgCast p1 var@(EParam pParam name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        let pgTy = getPgType env.database identifier
        let hsType = getHsType env.database pgTy
        introduceParameter env $
            Parameter
                { position = pParam
                , identifier = name
                , paramType = SimpleParameter
                , info =
                    Just
                        ParameterInfo
                            { pgType = pgTy
                            , hsType = hsType
                            , nullable = NonNullable
                            }
                }
        let typeInfo = TypeInfo pgTy NonNullable (Just name) hsType
        let tyVar = EParam pParam name typeInfo
        return $ EPgCast p1 tyVar p2 identifier typeInfo
    (EPgCast p1 var@(EParamMaybe pParam name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        let pgTy = getPgType env.database identifier
        let hsTy = getHsType env.database pgTy
        introduceParameter env $
            Parameter
                { position = pParam
                , identifier = name
                , paramType = SimpleMaybeParameter
                , info =
                    Just
                        ParameterInfo
                            { pgType = pgTy
                            , hsType = hsTy
                            , nullable = Nullable
                            }
                }
        let typeInfo = TypeInfo pgTy Nullable (Just name) hsTy
        let tyVar = EParamMaybe pParam name typeInfo
        return $ EPgCast p1 tyVar p2 identifier typeInfo
    (EPgCast p1 expr p2 identifier ()) -> do
        tyExpr <- tcExpr env expr
        -- FIXME check text, check if can be casted
        -- FIXME preserve IsNullable from underlying type
        let pgTy = getPgType env.database identifier
        let hsTy = getHsType env.database pgTy
        return $
            EPgCast
                p1
                tyExpr
                p2
                identifier
                (TypeInfo pgTy Nullable (Just identifier) hsTy)
    (EParens p1 expr p2 ()) -> do
        tyExpr <- tcExpr env expr
        let innerTy = exprType tyExpr
        return $ EParens p1 tyExpr p2 innerTy
    (EParam p name ()) -> throw env.compileError $ ParameterWithoutCastError p name
    (EParamMaybe p name ()) -> throw env.compileError $ MaybeParameterWithoutCastError p name
    (ELit p litVal _) -> case litVal of
        NumericLiteral -> do
            let hsTy = getHsType env.database PgNumeric
            return $
                ELit
                    p
                    litVal
                    (TypeInfo PgNumeric NonNullable Nothing hsTy)
        TextLiteral _ -> do
            let hsTy = getHsType env.database PgText
            return $ ELit p litVal (TypeInfo PgText NonNullable Nothing hsTy) -- FIXME this is 'unknown'
        (BoolLiteral _) -> do
            let hsTy = getHsType env.database PgBoolean
            return $ ELit p litVal (TypeInfo PgBoolean NonNullable Nothing hsTy)
    (ECol p colName _) -> do
        fields <- get env.fields
        case getColumnByName fields colName of
            [] -> throw env.compileError $ ColumnDoesNotExist p colName
            [envCol] -> do
                let hsTy = getHsType env.database envCol.typeName
                return $
                    ECol
                        p
                        colName
                        (TypeInfo envCol.typeName envCol.nullable (Just envCol.label) hsTy)
            ts -> throw env.compileError $ ColumnNameIsAmbigious p colName
    expr@(ENot p not innerExpr) -> do
        tyExpr <- tcExpr env innerExpr
        let ty = exprType tyExpr
        when (ty.pgType /= PgBoolean) do
            throw env.compileError $ ConditionTypeError tyExpr "NOT"
        return $ ENot p not tyExpr
    (EGuardedAnd lhs p1 identifier rhs p2) -> do
        introduceParameter env $
            Parameter
                { position = p1
                , identifier = identifier
                , paramType = GuardParameter
                , info = Nothing
                }
        (tyLhs, tyRhs) <- tyMustBeBoolean env "AND" lhs rhs
        return $ EGuardedAnd tyLhs p1 identifier tyRhs p2
    (EAnd p lhs and rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean env "AND" lhs rhs
        return $ EAnd p tyLhs and tyRhs
    (EOr p lhs or rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean env "OR" lhs rhs
        return $ EOr p tyLhs or tyRhs
    (EBinOp p lhs op rhs ()) -> do
        tcLhs <- tcExpr env lhs
        tcRhs <- tcExpr env rhs
        let (TypeInfo tyLhs nullableLhs _ _) = exprType tcLhs
        let (TypeInfo tyRhs nullableRhs _ _) = exprType tcRhs
        let nullableRes = tcNullable nullableLhs nullableRhs
        case getBinaryOpResult env.database tyLhs op tyRhs of
            Just tyRes -> do
                let hsTy = getHsType env.database tyRes
                return $ EBinOp p tcLhs op tcRhs (TypeInfo tyRes nullableRes Nothing hsTy)
            Nothing -> throw env.compileError $ OperatorDoesntExist p tyLhs op tyRhs
    (EBetween p lhs between rhs1 and rhs2) -> do
        -- TODO typecheck `between` against <= >=
        tyLhs <- tcExpr env lhs
        tyRhs1 <- tcExpr env rhs1
        tyRhs2 <- tcExpr env rhs2
        EBetween p
            <$> tcExpr env lhs
            <*> pure between
            <*> tcExpr env rhs1
            <*> pure and
            <*> tcExpr env rhs2
    (ENotBetween p lhs not between rhs1 and rhs2) ->
        ENotBetween p
            <$> tcExpr env lhs
            <*> pure not
            <*> pure between
            <*> tcExpr env rhs1
            <*> pure and
            <*> tcExpr env rhs2
  where
    tyMustBeBoolean
        :: (e :> es)
        => EnvE e
        -> String
        -> Expr ()
        -> Expr ()
        -> Eff es (Expr TypeInfo, Expr TypeInfo)
    tyMustBeBoolean env func lhs rhs = do
        tyLhs <- tcExpr env lhs
        tyRhs <- tcExpr env rhs
        when ((exprType tyLhs).pgType /= PgBoolean) do
            throw env.compileError $ ArgumentTypeError tyLhs func PgBoolean
        when ((exprType tyRhs).pgType /= PgBoolean) do
            throw env.compileError $ ArgumentTypeError tyRhs func PgBoolean
        return (tyLhs, tyRhs)

addTableToEnv :: (e :> es) => State [Field] e -> Table -> Eff es ()
addTableToEnv fieldsS table =
    addFieldsToEnv fieldsS . map (toEnvElem table.name) . columns $ table
  where
    toEnvElem :: Identifier -> Column -> Field
    toEnvElem alias column =
        Field
            { alias = alias
            , label = column.name
            , typeName = column.typeName
            , nullable = column.nullable
            }
