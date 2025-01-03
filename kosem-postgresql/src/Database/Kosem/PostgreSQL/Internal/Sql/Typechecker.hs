{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE NoMonoLocalBinds #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Database.Kosem.PostgreSQL.Internal.Sql.Typechecker where

import Bluefin.Compound
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
import Database.Kosem.PostgreSQL.Internal.PgType qualified as PgType
import Database.Kosem.PostgreSQL.Internal.Sql.Ast
import Database.Kosem.PostgreSQL.Internal.Sql.Parser
import Database.Kosem.PostgreSQL.Internal.Sql.Types
import Database.Kosem.PostgreSQL.Internal.Types
import Database.Kosem.PostgreSQL.Schema.Internal.Parser
import GHC.Data.Maybe (listToMaybe)
import Language.Haskell.TH.Syntax (Name)
import Text.Megaparsec (parseMaybe, parseTest)

data IntroType
    = Subquery
    | Join
    deriving (Show)

data Field = Field
    { alias :: Identifier
    , label :: Identifier
    , typeName :: PgType
    , nullable :: IsNullable
    }
    deriving (Show)

data TypecheckerEnv e = MkTypecheckerEnv
    { database :: Database
    , fields :: State [Field] e
    , commandInput :: State [CommandInput] e
    , compileError :: Exception CompileError e
    }

run
    :: (e :> es)
    => Exception CompileError e
    -> Database
    -> STerm ()
    -> String
    -> Eff es (CommandInfo CommandOutput)
run ex database ast input = do
    runTypecheckerEnv database ex [] [] \(env :: TypecheckerEnv e) -> do
        typedAst <- typecheck env ast
        -- TODO add to typecheck result
        fields <- get @e env.fields
        -- TODO add to typecheck result
        commandInput <- get @e env.commandInput
        pgTypes <- resultFromAst env typedAst
        let commandInputSorted = sortBy (comparing commandInputPosition) commandInput
        return $
            CommandInfo
                { input = commandInputSorted
                , output = pgTypes
                , rawCommand = input
                }

resultFromAst
    :: (e :> es)
    => TypecheckerEnv e
    -> STerm TypeInfo
    -> Eff es (NonEmpty CommandOutput)
resultFromAst env = \cases
    (Select resultColumns _ _) ->
        traverse (resultToCommandOutput env) resultColumns
  where
    resultToCommandOutput
        :: (e :> es)
        => TypecheckerEnv e
        -> AliasedExpr TypeInfo
        -> Eff es CommandOutput
    resultToCommandOutput env = \case
        WithAlias expr alias -> do
            let typeInfo = exprType expr
            -- FIXME this is not exacly correct,
            -- unknown might be resolved by `UNION`
            -- and we don't check if it is a string literal
            -- but this whole function has to be rewritten.
            let pgType =
                    if typeInfo.pgType == PgType.Unknown
                        then PgType.Text
                        else typeInfo.pgType
            return $ MkCommandOutput alias pgType typeInfo.nullable
        WithoutAlias expr -> do
            let typeInfo = exprType expr
            case typeInfo.identifier of
                Just identifier ->
                    return $
                        MkCommandOutput
                            { coIdentifier = identifier
                            , coPgType = typeInfo.pgType
                            , coNullable = typeInfo.nullable
                            }
                Nothing -> throw env.compileError $ ExprWithNoAlias expr

runTypecheckerEnv
    :: (e1 :> es)
    => Database
    -> Exception CompileError e1
    -> [Field]
    -> [CommandInput]
    -> (forall e. TypecheckerEnv e -> Eff (e :& es) r)
    -> Eff es r
runTypecheckerEnv database ex fields cInput action =
    evalState fields $ \fieldsS -> do
        evalState cInput $ \cInputS -> do
            useImplIn
                action
                MkTypecheckerEnv
                    { database = database
                    , fields = mapHandle fieldsS
                    , commandInput = mapHandle cInputS
                    , compileError = mapHandle ex
                    }

typecheck
    :: (e :> es) => TypecheckerEnv e -> STerm () -> Eff es (STerm TypeInfo)
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
    => TypecheckerEnv e
    -> Maybe (Where ())
    -> Eff es (Maybe (Where TypeInfo))
tcWhereClause env = \cases
    Nothing -> return Nothing
    (Just (Where expr)) -> do
        tyExpr <- tcExpr env expr
        when ((exprType tyExpr).pgType /= PgType.Boolean) do
            throw env.compileError $ ConditionTypeError tyExpr "WHERE"
        return $ Just $ Where tyExpr

tcSelectExpr
    :: (e :> es)
    => TypecheckerEnv e
    -> NonEmpty (AliasedExpr ())
    -> Eff es (NonEmpty (AliasedExpr TypeInfo))
tcSelectExpr env = mapM (tc env)
  where
    tc
        :: (e :> es)
        => TypecheckerEnv e
        -> AliasedExpr ()
        -> Eff es (AliasedExpr TypeInfo)
    tc env = \cases
        (WithAlias expr colAlias) -> do
            tcExpr <- tcExpr env expr
            return $ WithAlias tcExpr colAlias
        (WithoutAlias expr) -> do
            tcExpr <- tcExpr env expr
            return $ WithoutAlias tcExpr

tcFromItem
    :: (e :> es)
    => TypecheckerEnv e
    -> FromItem ()
    -> Eff es (FromItem TypeInfo)
tcFromItem env = \cases
    (FiTableName p tableName alias) -> do
        fiTable <- case getTableByName env.database tableName of
            [] -> throw env.compileError $ TableDoesNotExist p tableName
            [table] -> do
                addTableToEnv env.fields table alias
                return $ FiTableName p tableName alias
            ts -> throw env.compileError $ TableNameIsAmbiguous p tableName
        return fiTable
    (FiJoin lhs joinType rhs joinCondition) -> do
        tyLhs <- tcFromItem env lhs
        tyRhs <- tcFromItem env rhs
        tyJoinCondition <- tcJoinCondition env joinCondition
        return $ FiJoin tyLhs joinType tyRhs tyJoinCondition

tcJoinCondition
    :: (e :> es)
    => TypecheckerEnv e
    -> JoinCondition ()
    -> Eff es (JoinCondition TypeInfo)
tcJoinCondition env = \cases
    JcUsing -> return JcUsing
    (JcOn expr) -> do
        tyExpr <- tcExpr env expr
        when ((exprType tyExpr).pgType /= PgType.Boolean) do
            throw env.compileError $ ConditionTypeError tyExpr "JOIN/ON"
        return $ JcOn tyExpr

-- exprSetType :: Expr TypeInfo -> TypeInfo -> Expr TypeInfo

exprType :: Expr TypeInfo -> TypeInfo
-- TODO all those `Nullable` should be inferred
-- `NonNullable AND NonNullable` is NonNullable
-- so final type should be part of the constructor.
exprType = \cases
    (EPgCast _ _ _ _ ty) -> ty
    (EParens _ _ _ ty) -> ty
    (EFunction _ _ _ ty) -> ty
    (EParam _ _ ty) -> ty
    (EParamMaybe _ _ ty) -> ty
    (EGuardedBoolAnd{}) -> TypeInfo PgType.Boolean Nullable Nothing
    (EGuardedMaybeAnd{}) -> TypeInfo PgType.Boolean Nullable Nothing
    (ELiteral _ _ ty) -> ty
    (ECol _ _ _ ty) -> ty
    (ENot{}) -> TypeInfo PgType.Boolean Nullable Nothing
    (EAnd{}) -> TypeInfo PgType.Boolean Nullable Nothing
    (EOr{}) -> TypeInfo PgType.Boolean Nullable Nothing
    (EBinOp _ _ _ _ ty) -> ty
    (EUnaryOp _ _ _ ty) -> ty
    (EBetween{}) -> TypeInfo PgType.Boolean Nullable Nothing
    (ENotBetween{}) -> TypeInfo PgType.Boolean Nullable Nothing

tcNullable :: IsNullable -> IsNullable -> IsNullable
tcNullable = \cases
    NonNullable NonNullable -> NonNullable
    _ _ -> Nullable

tcExpr
    :: (e :> es)
    => TypecheckerEnv e
    -> Expr ()
    -> Eff es (Expr TypeInfo)
tcExpr env = \cases
    (EPgCast p1 var@(EParam pParam name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        let pgTy = getPgType env.database identifier
        introduceCommandInput env $
            CommandParameter $
                Parameter
                    { position = pParam
                    , pIdentifier = name
                    , paramType = SimpleParameter
                    , pPgType = pgTy
                    , pNullable = NonNullable
                    }
        let typeInfo = TypeInfo pgTy NonNullable (Just name.hsIdentifier)
        let tyVar = EParam pParam name typeInfo
        return $ EPgCast p1 tyVar p2 identifier typeInfo
    (EPgCast p1 var@(EParamMaybe pParam name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        let pgTy = getPgType env.database identifier
        introduceCommandInput env $
            CommandParameter $
                Parameter
                    { position = pParam
                    , pIdentifier = name
                    , paramType = SimpleMaybeParameter
                    , pPgType = pgTy
                    , pNullable = Nullable
                    }
        let typeInfo = TypeInfo pgTy Nullable (Just name.hsIdentifier)
        let tyVar = EParamMaybe pParam name typeInfo
        return $ EPgCast p1 tyVar p2 identifier typeInfo
    (EPgCast p1 expr p2 identifier ()) -> do
        tyExpr <- tcExpr env expr
        -- FIXME check text, check if can be casted
        let (TypeInfo _ isNullable _) = exprType tyExpr
        let pgTy = getPgType env.database identifier
        return $
            EPgCast
                p1
                tyExpr
                p2
                identifier
                (TypeInfo pgTy isNullable (Just identifier))
    (EParens p1 expr p2 ()) -> do
        tyExpr <- tcExpr env expr
        let innerTy = exprType tyExpr
        return $ EParens p1 tyExpr p2 innerTy
    (EParam p name ()) -> throw env.compileError $ ParameterWithoutCastError p name.hsIdentifier
    (EParamMaybe p name ()) -> throw env.compileError $ MaybeParameterWithoutCastError p name.hsIdentifier
    (EFunction p name exprs ()) -> do
        tyExprs <- traverse (tcExpr env) exprs
        let tyInfos = map exprType tyExprs
        let argsPgTypes = map (.pgType) tyInfos
        let mbFuncTy =
                getFunctionTy env.database name argsPgTypes
        -- \ FIXME there are exceptions to this tho
        -- `concat(null)` returns text always
        -- maybe some functions return error on null arg?
        let resNullable =
                if any (\x -> x.nullable == Nullable) tyInfos
                    then Nullable
                    else NonNullable
        case mbFuncTy of
            Nothing -> throw env.compileError $ FunctionDoesNotExist p name argsPgTypes
            Just ty -> do
                return $ EFunction p name tyExprs (TypeInfo ty resNullable Nothing)
    (ECol p mbAlias colName _) -> do
        fields <- get env.fields
        -- FIXME use alias
        case getColumnByName fields mbAlias colName of
            [] -> throw env.compileError $ ColumnDoesNotExist p mbAlias colName
            [envCol] -> do
                return $
                    ECol
                        p
                        mbAlias
                        colName
                        (TypeInfo envCol.typeName envCol.nullable (Just envCol.label))
            ts -> throw env.compileError $ ColumnNameIsAmbiguous p colName
    expr@(ENot p innerExpr) -> do
        tyExpr <- tcExpr env innerExpr
        let ty = exprType tyExpr
        when (ty.pgType /= PgType.Boolean) do
            throw env.compileError $ ConditionTypeError tyExpr "NOT"
        return $ ENot p tyExpr
    (EGuardedBoolAnd lhs p1 identifier pOpen rhs pClose) -> do
        introduceCommandInput env $
            CommandGuard $
                Guard
                    { guardPos = p1
                    , openBracketPos = pOpen
                    , closeBracketPos = pClose
                    , gIdentifier = identifier
                    , guardType = BooleanGuard
                    }
        (tyLhs, tyRhs) <- tyMustBeBoolean env "AND" lhs rhs
        return $ EGuardedBoolAnd tyLhs p1 identifier pOpen tyRhs pClose
    (EGuardedMaybeAnd lhs p1 identifier pOpen rhs pClose) -> do
        introduceCommandInput env $
            CommandGuard $
                Guard
                    { guardPos = p1
                    , openBracketPos = pOpen
                    , closeBracketPos = pClose
                    , gIdentifier = identifier
                    , guardType = MaybeGuard
                    }
        (tyLhs, tyRhs) <- tyMustBeBoolean env "AND" lhs rhs
        return $ EGuardedMaybeAnd tyLhs p1 identifier pOpen tyRhs pClose
    (EAnd p lhs rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean env "AND" lhs rhs
        return $ EAnd p tyLhs tyRhs
    (EOr p lhs rhs) -> do
        (tyLhs, tyRhs) <- tyMustBeBoolean env "OR" lhs rhs
        return $ EOr p tyLhs tyRhs
    (EBinOp p lhs op rhs ()) -> do
        tcLhs <- tcExpr env lhs
        tcRhs <- tcExpr env rhs
        let (TypeInfo tyLhs nullableLhs _) = exprType tcLhs
        let (TypeInfo tyRhs nullableRhs _) = exprType tcRhs
        let nullableRes = tcNullable nullableLhs nullableRhs
        case resolveBinOperatorType env.database tyLhs op tyRhs of
            Just (resolvedLhsTy, resolvedRhsTy, resTy) ->
                return $ EBinOp p tcLhs op tcRhs (TypeInfo resTy nullableRes Nothing)
            Nothing -> throw env.compileError $ OperatorDoesntExist p tyLhs op tyRhs
    (EUnaryOp p "-" lit@(ELiteral _ (IntegerLiteral val) ()) ()) -> do
        -- \| Postgres is being smart about integer literals type.
        -- select pg_typeof(2147483647); -- integer
        -- select pg_typeof(2147483648); -- bigint
        -- select pg_typeof(-2147483648); -- integer
        -- Last select should be (-) unary operator applied to `bigint`
        -- which would be of type `bigint` but number -2147483648
        -- is in the range of `integer` type, so the type is `integer`.
        -- The same thing happens on the boundary between `bigint` and `numeric`.
        -- We merge operator (-) with integer literal, negating it's sign.
        -- TODO document this somewhere
        let minusVal = negate val
        if
            | val > 9223372036854775808 ->
                return $
                    ELiteral
                        p
                        (IntegerLiteral minusVal)
                        (TypeInfo PgType.Numeric NonNullable Nothing)
            | val > 2147483648 ->
                return $ ELiteral p (IntegerLiteral minusVal) (TypeInfo PgType.Bigint NonNullable Nothing)
            | otherwise ->
                return $ ELiteral p (IntegerLiteral minusVal) (TypeInfo PgType.Integer NonNullable Nothing)
    (EUnaryOp p op rhs ()) -> do
        tcRhs <- tcExpr env rhs
        let (TypeInfo tyRhs nullableRhs _) = exprType tcRhs
        case resolveUnaryOperatorType env.database op tyRhs of
            Just (resolvedRhsTy, resTy) ->
                return $ EUnaryOp p op tcRhs (TypeInfo resTy nullableRhs Nothing)
            Nothing -> throw env.compileError $ UnaryOperatorDoesntExist p op tyRhs
    (EBetween p lhs rhs1 rhs2) -> do
        -- TODO typecheck `between` against <= >=
        tyLhs <- tcExpr env lhs
        tyRhs1 <- tcExpr env rhs1
        tyRhs2 <- tcExpr env rhs2
        EBetween p
            <$> tcExpr env lhs
            <*> tcExpr env rhs1
            <*> tcExpr env rhs2
    (ENotBetween p lhs rhs1 rhs2) ->
        ENotBetween p
            <$> tcExpr env lhs
            <*> tcExpr env rhs1
            <*> tcExpr env rhs2
    (ELiteral p litVal _) -> case litVal of
        NonIntegerNumberLiteral x -> do
            return $
                ELiteral
                    p
                    litVal
                    (TypeInfo PgType.Numeric NonNullable Nothing)
        IntegerLiteral val -> do
            if
                | val > 9223372036854775807 ->
                    return $ ELiteral p litVal (TypeInfo PgType.Numeric NonNullable Nothing)
                | val > 2147483647 ->
                    return $ ELiteral p litVal (TypeInfo PgType.Bigint NonNullable Nothing)
                | otherwise ->
                    return $ ELiteral p litVal (TypeInfo PgType.Integer NonNullable Nothing)
        StringLiteral _ -> do
            return $ ELiteral p litVal (TypeInfo PgType.Unknown NonNullable Nothing)
        (BoolLiteral _) -> do
            return $ ELiteral p litVal (TypeInfo PgType.Boolean NonNullable Nothing)
  where
    tyMustBeBoolean
        :: (e :> es)
        => TypecheckerEnv e
        -> String
        -> Expr ()
        -> Expr ()
        -> Eff es (Expr TypeInfo, Expr TypeInfo)
    tyMustBeBoolean env func lhs rhs = do
        tyLhs <- tcExpr env lhs
        tyRhs <- tcExpr env rhs
        when ((exprType tyLhs).pgType /= PgType.Boolean) do
            throw env.compileError $ ArgumentTypeError tyLhs func PgType.Boolean
        when ((exprType tyRhs).pgType /= PgType.Boolean) do
            throw env.compileError $ ArgumentTypeError tyRhs func PgType.Boolean
        return (tyLhs, tyRhs)

addTableToEnv :: (e :> es) => State [Field] e -> Table -> Alias -> Eff es ()
addTableToEnv fieldsS table alias =
    addFieldsToEnv fieldsS . map (toTypecheckerEnvlem alias) . columns $ table
  where
    toTypecheckerEnvlem :: Alias -> Column -> Field
    toTypecheckerEnvlem alias column =
        Field
            { alias = alias
            , label = column.name
            , typeName = column.typeName
            , nullable = column.nullable
            }

introduceCommandInput
    :: (e :> es)
    => TypecheckerEnv e
    -> CommandInput
    -> Eff es ()
introduceCommandInput env commandInput =
    modify env.commandInput (<> [commandInput])

getFunctionTy
    :: Database
    -> Identifier
    -> [PgType]
    -> Maybe PgType
getFunctionTy database functionId argsTy =
    listToMaybe
        . map (\(_, _, ty) -> ty)
        . filter (\(_, a, _) -> a == argsTy)
        . filter (\(o, _, _) -> o == functionId)
        $ database.functions

getPgType :: Database -> Identifier -> PgType
getPgType database identifier = find identifier database.typesL
  where
    find :: Identifier -> [PgType] -> PgType
    find identifier = \cases
        -- TODO proper error
        [] -> error $ "no type: " <> show identifier
        (x : xs) ->
            if x.name == identifier || x.nameSql == identifier
                then x
                else find identifier xs

addFieldsToEnv
    :: (e :> es)
    => State [Field] e
    -> [Field]
    -> Eff es ()
addFieldsToEnv fieldsS fields =
    modify fieldsS (<> fields)

getTableByName :: Database -> Identifier -> [Table]
getTableByName database tableName =
    (filter (\table -> table.name == tableName) . tables) database

getColumnByName :: [Field] -> Maybe Alias -> Identifier -> [Field]
getColumnByName fields mbAlias name = do
    let matchesAlias = maybe (const True) (==) mbAlias
    filter (\e -> e.label == name)
        . filter (\e -> matchesAlias e.alias)
        $ fields

resolveBinOperatorType
    :: Database
    -> PgType
    -> Operator
    -> PgType
    -> Maybe (PgType, PgType, PgType)
resolveBinOperatorType database lhs op rhs = do
    -- \| Algorithm explained here:
    -- https://www.postgresql.org/docs/17/typeconv-oper.html#TYPECONV-OPER

    -- \| Postgres converts '!=' to '<>', see note:
    -- https://www.postgresql.org/docs/current/functions-comparison.html
    let realOp = if op == "!=" then "<>" else op
        candidateBinOps =
            filter (\(o, _, _, _) -> o == realOp) database.binaryOps
    let assumedLhsTy = if lhs == PgType.Unknown then rhs else lhs
    let assumedRhsTy = if rhs == PgType.Unknown then lhs else rhs
    listToMaybe
        . map (\(_, _, _, ty) -> (assumedLhsTy, assumedRhsTy, ty))
        . filter (\(_, _, r, _) -> r == assumedRhsTy)
        . filter (\(_, l, _, _) -> l == assumedLhsTy)
        $ candidateBinOps

resolveUnaryOperatorType
    :: Database
    -> Operator
    -> PgType
    -> Maybe (PgType, PgType)
resolveUnaryOperatorType database op rhs = do
    -- \| Algorithm explained here:
    -- https://www.postgresql.org/docs/17/typeconv-oper.html#TYPECONV-OPER

    -- \| Postgres converts '!=' to '<>', see note:
    -- https://www.postgresql.org/docs/current/functions-comparison.html
    let realOp = if op == "!=" then "<>" else op
        candidateUnaryOps =
            filter (\(o, _, _) -> o == realOp) database.unaryOps
    listToMaybe
        . map (\(_, _, ty) -> (rhs, ty))
        . filter (\(_, r, _) -> r == rhs)
        $ candidateUnaryOps

getBinaryOpResult
    :: Database
    -> PgType
    -> Operator
    -> PgType
    -> Maybe PgType
getBinaryOpResult database lhs op rhs = do
    -- \| Postgres converts '!=' to '<>', see note:
    -- https://www.postgresql.org/docs/current/functions-comparison.html
    let realOp = if op == "!=" then "<>" else op
        binOps = database.binaryOps
    listToMaybe
        . map (\(_, _, _, ty) -> ty)
        . filter (\(_, _, r, _) -> r == rhs)
        . filter (\(_, l, _, _) -> l == lhs)
        . filter (\(o, _, _, _) -> o == realOp)
        $ database.binaryOps
