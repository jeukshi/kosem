{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Type where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Ast
import Database.Kosem.PostgreSQL.Internal.Env
import Database.Kosem.PostgreSQL.Internal.Parser
import Database.Kosem.PostgreSQL.Schema.Internal.Parser
import Text.Megaparsec (parseMaybe, parseTest)

typecheck :: STerm () -> Tc (STerm SqlType)
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

tcWhereClause :: Maybe (Where ()) -> Tc (Maybe (Where SqlType))
tcWhereClause = \cases
    Nothing -> return Nothing
    (Just (Where expr)) -> do
        tyExpr <- tcExpr expr
        when (exprType tyExpr ~/=~ pgBoolNullable) do
            throwError $ Err "argument of 'WHERE' must be type 'boolean'"
        return $ Just $ Where tyExpr

tcSelectExpr
    :: NonEmpty (AliasedExpr ())
    -> Tc (NonEmpty (AliasedExpr SqlType))
tcSelectExpr = mapM tc
  where
    tc :: AliasedExpr () -> Tc (AliasedExpr SqlType)
    tc = \cases
        (WithAlias expr colAlias maybeAs) -> do
            tcExpr <- tcExpr expr
            return $ WithAlias tcExpr colAlias maybeAs
        (WithoutAlias expr) -> do
            tcExpr <- tcExpr expr
            return $ WithoutAlias tcExpr

tcFromItem :: FromItem () -> Tc (FromItem SqlType)
tcFromItem = \cases
    (FiTableName (TableName tableName)) -> do
        table <- tableByName tableName
        addTableToEnv table
        return $ FiTableName (TableName tableName)
    (FiJoin lhs joinType rhs joinCondition) -> do
        tyLhs <- tcFromItem lhs
        tyRhs <- tcFromItem rhs
        tyJoinCondition <- tcJoinCondition joinCondition
        return $ FiJoin tyLhs joinType tyRhs tyJoinCondition

tcJoinCondition :: JoinCondition () -> Tc (JoinCondition SqlType)
tcJoinCondition = \cases
    JcUsing -> return JcUsing
    (JcOn expr) -> do
        tyExpr <- tcExpr expr
        when (exprType tyExpr ~/=~ pgBoolNullable) do
            throwError $ Err "argument of 'JOIN/ON' must be type 'boolean'"
        return $ JcOn tyExpr

exprType :: Expr SqlType -> SqlType
exprType = \cases
    (EPgCast _ _ ty) -> ty
    (EParens _ ty) -> ty
    (EParam _ _ ty) -> ty
    (EParamMaybe _ _ ty) -> ty
    (ELit _ ty) -> ty
    (ECol _ ty) -> ty
    (ENot{}) -> pgBoolNullable
    (EAnd{}) -> pgBoolNullable
    (EOr{}) -> pgBoolNullable
    (ELessThan{}) -> pgBoolNullable
    (EGreaterThan{}) -> pgBoolNullable
    (ELessThanOrEqualTo{}) -> pgBoolNullable
    (EGreaterThanOrEqualTo{}) -> pgBoolNullable
    (EEqual{}) -> pgBoolNullable
    (ENotEqual{}) -> pgBoolNullable
    (EBetween{}) -> pgBoolNullable
    (ENotBetween{}) -> pgBoolNullable

tcExpr :: Expr () -> Tc (Expr SqlType)
tcExpr = \cases
    (EPgCast var@(EParam{}) ty ()) -> do
        -- FIXME check text, check if can be casted
        tyVar <-
            tcExpr var >>= \case
                (EParam no name _) ->
                    return $ EParam no name (Scalar (PgType ty) NonNullable)
                _ -> throwError $ Err "impossible!"
        return $ EPgCast tyVar ty (Scalar (PgType ty) NonNullable)
    (EPgCast var@(EParamMaybe{}) ty ()) -> do
        -- FIXME check text, check if can be casted
        tyVar <-
            tcExpr var >>= \case
                (EParamMaybe no name _) ->
                    return $ EParamMaybe no name (Scalar (PgType ty) Nullable)
                _ -> throwError $ Err "impossible!"
        return $ EPgCast tyVar ty (Scalar (PgType ty) Nullable)
    (EPgCast expr text ()) -> do
        tyExpr <- tcExpr expr
        -- FIXME check text, check if can be casted
        -- FIXME preserve IsNullable from underlying type
        return $ EPgCast tyExpr text (Scalar (PgType text) Nullable)
    (EParens expr ()) -> do
        tyExpr <- tcExpr expr
        let innerTy = exprType tyExpr
        return $ EParens tyExpr innerTy
    (EParam _ name ()) -> do
        paramNumber <-
            getParamNumber name >>= \case
                Just ix -> return ix
                Nothing -> addParam name
        return $ EParam paramNumber name (UnknownParam NonNullable)
    (EParamMaybe _ name ()) -> do
        paramNumber <-
            getParamNumber name >>= \case
                Just ix -> return ix
                Nothing -> addParam name
        return $ EParamMaybe paramNumber name (UnknownParam Nullable)
    (ELit litVal _) -> case litVal of
        NumericLiteral -> return $ ELit litVal (Scalar "numeric" NonNullable)
        TextLiteral _ -> return $ ELit litVal (Scalar "text" NonNullable)
        (BoolLiteral _) -> return $ ELit litVal (Scalar "boolean" NonNullable)
    (ECol colName _) -> do
        envCol <- columnByName colName
        -- FIXME NonNullable from colDef
        return $ ECol colName (Scalar envCol.typeName NonNullable)
    (ENot not expr) -> do
        tyExpr <- tcExpr expr
        let ty = exprType tyExpr
        when (ty ~/=~ pgBoolNullable) do
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
    tyMustBeBoolean :: Text -> Expr () -> Expr () -> Tc (Expr SqlType, Expr SqlType)
    tyMustBeBoolean func lhs rhs = do
        tyLhs <- tcExpr lhs
        tyRhs <- tcExpr rhs
        when (exprType tyLhs ~/=~ pgBoolNullable) do
            throwError $ Err $ "argument of '" <> func <> "' must be type 'boolean'"
        when (exprType tyRhs ~/=~ pgBoolNullable) do
            throwError $ Err $ "argument of '" <> func <> "' must be type 'boolean'"
        return (tyLhs, tyRhs)

    tyMustBeEqual :: Text -> Expr () -> Expr () -> Tc (Expr SqlType, Expr SqlType)
    tyMustBeEqual func lhs rhs = do
        tyLhs <- tcExpr lhs
        tyRhs <- tcExpr rhs
        when (exprType tyLhs ~/=~ exprType tyRhs) do
            throwError $ Err $ "arguments of '" <> func <> "' must be of the same type"
        return (tyLhs, tyRhs)

columnByName :: Text -> Tc Field
columnByName name =
    getColumnByName name >>= \case
        [] -> throwError $ Err ("column does not exist: " <> name)
        [t] -> return t
        ts -> throwError $ Err ("Column name is ambigious: " <> name)

tableByName :: Text -> Tc Table
tableByName name =
    getTableByName name >>= \case
        [] -> throwError $ Err "relation does not exist"
        [t] -> return t
        ts -> throwError $ Err "Table name is ambigious"

addTableToEnv :: Table -> Tc ()
addTableToEnv table =
    addFieldsToEnv . map (toEnvElem table.name) . columns $ table
  where
    toEnvElem :: Text -> Column -> Field
    toEnvElem alias column =
        Field
            { alias = alias
            , label = column.name
            , typeName = column.typeName
            }
