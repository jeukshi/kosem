{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Type where

import Control.Monad (when)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Ast
import Database.Kosem.PostgreSQL.Internal.Diagnostics (
    CompileError (..),
    DiagnosticSpan (..),
    P,
    combineSpans,
    movePby,
 )
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
            throwError $ TypeError (toDiagnosticSpan tyExpr) "argument of 'WHERE' must be type 'boolean'"
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
            throwError $ TypeError (toDiagnosticSpan tyExpr) "argument of 'JOIN/ON' must be type 'boolean'"
        return $ JcOn tyExpr

toDiagnosticSpan :: Expr a -> DiagnosticSpan P
toDiagnosticSpan = \cases
    (EPgCast p1 _ p2 identifier _) ->
        DiagnosticSpan
            p1
            (p2 `movePby` identifierLength identifier)
    (EParens p1 _ p2 _) ->
        DiagnosticSpan p1 p2
    (EParam p _ identifier _) ->
        DiagnosticSpan
            p
            -- \| +1 from ':' prefix.
            (p `movePby` (identifierLength identifier + 1))
    (EParamMaybe p _ identifier _) ->
        DiagnosticSpan
            p
            -- \| +2 from ':?' prefix.
            (p `movePby` (identifierLength identifier + 2))
    (ELit p lit _) -> case lit of
        NumericLiteral -> undefined -- TODO
        BoolLiteral text ->
            DiagnosticSpan
                p
                (p `movePby` T.length text)
        TextLiteral text ->
            DiagnosticSpan
                p
                -- \| +2 from single quote.
                (p `movePby` (T.length text + 2))
    (ECol p identifier _) ->
        DiagnosticSpan
            p
            (p `movePby` identifierLength identifier)
    (ENot p _ expr) ->
        DiagnosticSpan p p
            `combineSpans` toDiagnosticSpan expr
    (EAnd _ lhs _ rhs) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs
    (EOr _ lhs _ rhs) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs
    (EBinOp _ lhs _ rhs _) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs
    (EBetween _ lhs _ _ _ rhs2) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs2
    (ENotBetween _ lhs _ _ _ _ rhs2) ->
        toDiagnosticSpan lhs
            `combineSpans` toDiagnosticSpan rhs2

exprType :: Expr TypeInfo -> TypeInfo
exprType = \cases
    (EPgCast _ _ _ _ ty) -> ty
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
    (EPgCast p1 var@(EParam pParam _ name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        ty <- getType identifier
        paramNumber <-
            getParamNumber name >>= \case
                Just ix -> return ix
                Nothing -> addParam name
        let typeInfo = TypeInfo ty NonNullable
        let tyVar = EParam pParam paramNumber name typeInfo
        return $ EPgCast p1 tyVar p2 identifier typeInfo
    (EPgCast p1 var@(EParamMaybe pParam _ name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        ty <- getType identifier
        paramNumber <-
            getParamNumber name >>= \case
                Just ix -> return ix
                Nothing -> addParam name
        let typeInfo = TypeInfo ty Nullable
        let tyVar = EParamMaybe pParam paramNumber name typeInfo
        return $ EPgCast p1 tyVar p2 identifier typeInfo
    (EPgCast p1 expr p2 identifier ()) -> do
        tyExpr <- tcExpr expr
        -- FIXME check text, check if can be casted
        -- FIXME preserve IsNullable from underlying type
        ty <- getType identifier
        return $ EPgCast p1 tyExpr p2 identifier (TypeInfo ty Nullable)
    (EParens p1 expr p2 ()) -> do
        tyExpr <- tcExpr expr
        let innerTy = exprType tyExpr
        return $ EParens p1 tyExpr p2 innerTy
    expr@(EParam p _ name ()) ->
        -- TODO
        throwError $
            ParametersWithoutCastError (toDiagnosticSpan expr) "parameters without cast are not supported"
    expr@(EParamMaybe _ _ name ()) -> do
        -- TODO
        throwError $
            ParametersWithoutCastError (toDiagnosticSpan expr) "parameters without cast are not supported"
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
            throwError $ TypeError (toDiagnosticSpan expr) "argument of 'NOT' must be type 'boolean'"
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
        let nullableRes = tcNullable nullableLhs nullableRhs
        getBinaryOpResult tyLhs op tyRhs >>= \case
            Just tyRes ->
                return $ EBinOp p tcLhs op tcRhs (TypeInfo tyRes nullableRes)
            Nothing ->
                throwError $
                    OperatorDoesntExist
                        (DiagnosticSpan p (p `movePby` operatorLength op))
                        ("operator does not exist: "
                        <> pgTypePretty tyLhs
                        <> " "
                        <> operatorPretty op
                        <> " "
                        <> pgTypePretty tyRhs
                        )
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
            throwError $
                TypeError (toDiagnosticSpan tyLhs) $
                    "argument of '" <> func <> "' must be type 'boolean'"
        when (exprType tyRhs ~/=~ TypeInfo PgBoolean Nullable) do
            throwError $
                TypeError (toDiagnosticSpan tyRhs) $
                    "argument of '" <> func <> "' must be type 'boolean'"
        return (tyLhs, tyRhs)

    tyMustBeEqual :: Text -> Expr () -> Expr () -> Tc (Expr TypeInfo, Expr TypeInfo)
    tyMustBeEqual func lhs rhs = do
        tyLhs <- tcExpr lhs
        tyRhs <- tcExpr rhs
        when (exprType tyLhs ~/=~ exprType tyRhs) do
            -- TODO prolly not tyLhs
            throwError $
                TypeError (toDiagnosticSpan tyLhs) $
                    "arguments of '" <> func <> "' must be of the same type"
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
