{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Type where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Ast
import Database.Kosem.PostgreSQL.Internal.Env
import Database.Kosem.PostgreSQL.Internal.Parser
import Database.Kosem.PostgreSQL.Internal.PgBuiltin
import Database.Kosem.PostgreSQL.Internal.Types
import Database.Kosem.PostgreSQL.Schema.Internal.Parser
import Text.Megaparsec (parseMaybe, parseTest)
import Database.Kosem.PostgreSQL.Internal.Diagnostics (P, CompileError (..))

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
        when (exprType tyExpr ~/=~ TypeInfo PgBoolean Nullable) do
            throwError $ TypeError (exprPosition tyExpr)  "argument of 'WHERE' must be type 'boolean'"
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
    (FiTableName tableName) -> do
        table <- tableByName tableName
        addTableToEnv table
        return $ FiTableName tableName
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
        when (exprType tyExpr ~/=~ TypeInfo PgBoolean Nullable) do
            throwError $ TypeError (exprPosition tyExpr) "argument of 'JOIN/ON' must be type 'boolean'"
        return $ JcOn tyExpr

exprPosition :: Expr a -> P
exprPosition = \cases
    (EPgCast p _ _ _) -> p
    (EParens p _ _ _) -> p
    (EParam p _ _ _) -> p
    (EParamMaybe p _ _ _) -> p
    (ELit p _ _) -> p
    (ECol p _ _) -> p
    (ENot p _ _) -> p
    (EAnd p _ _ _) -> p
    (EOr p _ _ _) -> p
    (EBinOp p _ _ _ _) -> p
    (EBetween p _ _ _ _ _) -> p
    (ENotBetween p _ _ _ _ _ _) -> p

exprType :: Expr TypeInfo -> TypeInfo
exprType = \cases
    (EPgCast _ _ _ ty) -> ty
    (EParens _ _ _ ty) -> ty
    (EParam _ _ _ ty) -> ty
    (EParamMaybe _ _ _ ty) -> ty
    (ELit _ _ ty) -> ty
    (ECol _ _ ty) -> ty
    (ENot{}) -> TypeInfo PgBoolean Nullable
    (EAnd{}) -> TypeInfo PgBoolean Nullable
    (EOr{}) -> TypeInfo PgBoolean Nullable
    (EBinOp _ _ _ _ ty) -> ty
    (EBetween{}) -> TypeInfo PgBoolean Nullable
    (ENotBetween{}) -> TypeInfo PgBoolean Nullable

tcNullable :: IsNullable -> IsNullable -> IsNullable
tcNullable = \cases
    NonNullable NonNullable -> NonNullable
    _ _ -> Nullable

tcExpr :: Expr () -> Tc (Expr TypeInfo)
tcExpr = \cases
    (EPgCast p var@(EParam pParam _ name _) identifier ()) -> do
        -- FIXME check if can be casted
        ty <- getType identifier
        paramNumber <-
            getParamNumber name >>= \case
                Just ix -> return ix
                Nothing -> addParam name
        let typeInfo = TypeInfo ty NonNullable
        let tyVar = EParam pParam paramNumber name typeInfo
        return $ EPgCast p tyVar identifier typeInfo
    (EPgCast p var@(EParamMaybe pParam _ name _) identifier ()) -> do
        -- FIXME check if can be casted
        ty <- getType identifier
        paramNumber <-
            getParamNumber name >>= \case
                Just ix -> return ix
                Nothing -> addParam name
        let typeInfo = TypeInfo ty Nullable
        let tyVar = EParamMaybe pParam paramNumber name typeInfo
        return $ EPgCast p tyVar identifier typeInfo
    (EPgCast p expr identifier ()) -> do
        tyExpr <- tcExpr expr
        -- FIXME check text, check if can be casted
        -- FIXME preserve IsNullable from underlying type
        ty <- getType identifier
        return $ EPgCast p tyExpr identifier (TypeInfo ty Nullable)
    (EParens p1 expr p2 ()) -> do
        tyExpr <- tcExpr expr
        let innerTy = exprType tyExpr
        return $ EParens p1 tyExpr p2 innerTy
    expr@(EParam p _ name ()) ->
        -- TODO
        throwError $ ParametersWithoutCastError (exprPosition expr) "parameters without cast are not supported"
    expr@(EParamMaybe _ _ name ()) -> do
        -- TODO
        throwError $ ParametersWithoutCastError (exprPosition expr) "parameters without cast are not supported"
    (ELit p litVal _) -> case litVal of
        NumericLiteral -> return $ ELit p litVal (TypeInfo PgNumeric NonNullable)
        TextLiteral _ -> return $ ELit p litVal (TypeInfo PgText NonNullable) -- FIXME this is 'unknown'
        (BoolLiteral _) -> return $ ELit p litVal (TypeInfo PgBoolean NonNullable)
    (ECol p colName _) -> do
        envCol <- columnByName colName
        -- FIXME NonNullable from colDef
        return $ ECol p colName (TypeInfo envCol.typeName envCol.nullable)
    expr@(ENot p not innerExpr) -> do
        tyExpr <- tcExpr innerExpr
        let ty = exprType tyExpr
        when (ty ~/=~ TypeInfo PgBoolean Nullable) do
            throwError $ TypeError (exprPosition expr) "argument of 'NOT' must be type 'boolean'"
        return $ ENot p not tyExpr
    (EAnd p lhs and rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean "AND" lhs rhs
        return $ EAnd p tyLhs and tyRhs
    (EOr p lhs or rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean "OR" lhs rhs
        return $ EOr p tyLhs or tyRhs
    (EBinOp p lhs op rhs ()) -> do
        tcLhs <- tcExpr lhs
        tcRhs <- tcExpr rhs
        let (TypeInfo tyLhs nullableLhs) = exprType tcLhs
        let (TypeInfo tyRhs nullableRhs) = exprType tcRhs
        tyRes <- getBinaryOpResult tyLhs op tyRhs
        let nullableRes = tcNullable nullableLhs nullableRhs
        return $ EBinOp p tcLhs op tcRhs (TypeInfo tyRes nullableRes)
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
        when (exprType tyLhs ~/=~ TypeInfo PgBoolean Nullable) do
            throwError $ TypeError (exprPosition tyLhs) $ "argument of '" <> T.unpack func <> "' must be type 'boolean'"
        when (exprType tyRhs ~/=~ TypeInfo PgBoolean Nullable) do
            throwError $ TypeError (exprPosition tyRhs) $ "argument of '" <> T.unpack func <> "' must be type 'boolean'"
        return (tyLhs, tyRhs)

    tyMustBeEqual :: Text -> Expr () -> Expr () -> Tc (Expr TypeInfo, Expr TypeInfo)
    tyMustBeEqual func lhs rhs = do
        tyLhs <- tcExpr lhs
        tyRhs <- tcExpr rhs
        when (exprType tyLhs ~/=~ exprType tyRhs) do
            -- TODO prolly not tyLhs
            throwError $ TypeError (exprPosition tyLhs) $ "arguments of '" <> T.unpack func <> "' must be of the same type"
        return (tyLhs, tyRhs)

columnByName :: Identifier -> Tc Field
columnByName name =
    getColumnByName name >>= \case
        [] -> throwError $ error ("column does not exist: " <> show name)
        [t] -> return t
        ts -> throwError $ error ("Column name is ambigious: " <> show name)

tableByName :: Identifier -> Tc Table
tableByName name =
    getTableByName name >>= \case
        [] -> throwError $ error "relation does not exist"
        [t] -> return t
        ts -> throwError $ error "Table name is ambigious"

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
