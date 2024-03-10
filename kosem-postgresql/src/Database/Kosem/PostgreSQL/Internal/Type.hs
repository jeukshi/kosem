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
            throwError $ Err "argument of 'WHERE' must be type 'boolean'"
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
            throwError $ Err "argument of 'JOIN/ON' must be type 'boolean'"
        return $ JcOn tyExpr

exprType :: Expr TypeInfo -> TypeInfo
exprType = \cases
    (EPgCast _ _ ty) -> ty
    (EParens _ ty) -> ty
    (EParam _ _ ty) -> ty
    (EParamMaybe _ _ ty) -> ty
    (ELit _ ty) -> ty
    (ECol _ ty) -> ty
    (ENot{}) -> TypeInfo PgBoolean Nullable
    (EAnd{}) -> TypeInfo PgBoolean Nullable
    (EOr{}) -> TypeInfo PgBoolean Nullable
    (ELessThan{}) -> TypeInfo PgBoolean Nullable
    (EGreaterThan{}) -> TypeInfo PgBoolean Nullable
    (ELessThanOrEqualTo{}) -> TypeInfo PgBoolean Nullable
    (EGreaterThanOrEqualTo{}) -> TypeInfo PgBoolean Nullable
    (EEqual{}) -> TypeInfo PgBoolean Nullable
    (ENotEqual{}) -> TypeInfo PgBoolean Nullable
    (EBetween{}) -> TypeInfo PgBoolean Nullable
    (ENotBetween{}) -> TypeInfo PgBoolean Nullable

tcExpr :: Expr () -> Tc (Expr TypeInfo)
tcExpr = \cases
    (EPgCast var@(EParam _ name _) identifier ()) -> do
        -- FIXME check if can be casted
        ty <- getType identifier
        paramNumber <-
            getParamNumber name >>= \case
                Just ix -> return ix
                Nothing -> addParam name
        let typeInfo = TypeInfo ty NonNullable
        let tyVar = EParam paramNumber name typeInfo
        return $ EPgCast tyVar identifier typeInfo
    (EPgCast var@(EParamMaybe _ name _) identifier ()) -> do
        -- FIXME check if can be casted
        ty <- getType identifier
        paramNumber <-
            getParamNumber name >>= \case
                Just ix -> return ix
                Nothing -> addParam name
        let typeInfo = TypeInfo ty Nullable
        let tyVar = EParamMaybe paramNumber name typeInfo
        return $ EPgCast tyVar identifier typeInfo
    (EPgCast expr identifier ()) -> do
        tyExpr <- tcExpr expr
        -- FIXME check text, check if can be casted
        -- FIXME preserve IsNullable from underlying type
        ty <- getType identifier
        return $ EPgCast tyExpr identifier (TypeInfo ty Nullable)
    (EParens expr ()) -> do
        tyExpr <- tcExpr expr
        let innerTy = exprType tyExpr
        return $ EParens tyExpr innerTy
    (EParam _ name ()) ->
        -- TODO
        throwError $ Err "parameters without cast are not supported"
    (EParamMaybe _ name ()) -> do
        -- TODO
        throwError $ Err "parameters without cast are not supported"
    (ELit litVal _) -> case litVal of
        NumericLiteral -> return $ ELit litVal (TypeInfo PgNumeric NonNullable)
        TextLiteral _ -> return $ ELit litVal (TypeInfo PgText NonNullable) -- FIXME this is 'unknown'
        (BoolLiteral _) -> return $ ELit litVal (TypeInfo PgBoolean NonNullable)
    (ECol colName _) -> do
        envCol <- columnByName colName
        -- FIXME NonNullable from colDef
        return $ ECol colName (TypeInfo envCol.typeName NonNullable)
    (ENot not expr) -> do
        tyExpr <- tcExpr expr
        let ty = exprType tyExpr
        when (ty ~/=~ TypeInfo PgBoolean Nullable) do
            throwError $ Err "argument of 'NOT' must be type 'boolean'"
        return $ ENot not tyExpr
    (EAnd lhs and rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean "AND" lhs rhs
        return $ EAnd tyLhs and tyRhs
    (EOr lhs or rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean "OR" lhs rhs
        return $ EOr tyLhs or tyRhs
    (ELessThan lhs rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeEqual "<" lhs rhs
        return $ ELessThan tyLhs tyRhs
    (EGreaterThan lhs rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeEqual ">" lhs rhs
        return $ EGreaterThan tyLhs tyRhs
    (ELessThanOrEqualTo lhs rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeEqual "<=" lhs rhs
        return $ ELessThanOrEqualTo tyLhs tyRhs
    (EGreaterThanOrEqualTo lhs rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeEqual ">=" lhs rhs
        return $ EGreaterThanOrEqualTo tyLhs tyRhs
    (EEqual lhs rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeEqual "=" lhs rhs
        return $ EEqual tyLhs tyRhs
    (ENotEqual lhs style rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeEqual "<>" lhs rhs
        return $ ENotEqual tyLhs style tyRhs
    (EBetween lhs between rhs1 and rhs2) -> do
        -- TODO typecheck `between` against <= >=
        tyLhs <- tcExpr lhs
        tyRhs1 <- tcExpr rhs1
        tyRhs2 <- tcExpr rhs2
        EBetween
            <$> tcExpr lhs
            <*> pure between
            <*> tcExpr rhs1
            <*> pure and
            <*> tcExpr rhs2
    (ENotBetween lhs not between rhs1 and rhs2) ->
        ENotBetween
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
            throwError $ Err $ "argument of '" <> func <> "' must be type 'boolean'"
        when (exprType tyRhs ~/=~ TypeInfo PgBoolean Nullable) do
            throwError $ Err $ "argument of '" <> func <> "' must be type 'boolean'"
        return (tyLhs, tyRhs)

    tyMustBeEqual :: Text -> Expr () -> Expr () -> Tc (Expr TypeInfo, Expr TypeInfo)
    tyMustBeEqual func lhs rhs = do
        tyLhs <- tcExpr lhs
        tyRhs <- tcExpr rhs
        when (exprType tyLhs ~/=~ exprType tyRhs) do
            throwError $ Err $ "arguments of '" <> func <> "' must be of the same type"
        return (tyLhs, tyRhs)

columnByName :: Identifier -> Tc Field
columnByName name =
    getColumnByName name >>= \case
        [] -> throwError $ Err ("column does not exist: " <> T.pack (show name))
        [t] -> return t
        ts -> throwError $ Err ("Column name is ambigious: " <> T.pack (show name))

tableByName :: Identifier -> Tc Table
tableByName name =
    getTableByName name >>= \case
        [] -> throwError $ Err "relation does not exist"
        [t] -> return t
        ts -> throwError $ Err "Table name is ambigious"

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
            }
