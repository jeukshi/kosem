{-# LANGUAGE DuplicateRecordFields #-}
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
    -> Eff es CommandInfo
run ex database ast input = do
    runTypecheckerEnv database ex [] [] \(env :: TypecheckerEnv e) -> do
        typedAst <- typecheck env ast
        fields <- get @e env.fields
        commandInput <- get @e env.commandInput
        hsTypes <- resultFromAst env typedAst
        let commandInputSorted = sortBy (comparing commandInputPosition) commandInput
        return $
            CommandInfo
                { input = commandInputSorted
                , output = hsTypes
                , rawCommand = input
                }

resultFromAst
    :: (e :> es)
    => TypecheckerEnv e
    -> STerm TypeInfo
    -> Eff es (NonEmpty SqlMapping)
resultFromAst env = \cases
    (Select resultColumns _ _) ->
        traverse (resultToSqlMapping env) resultColumns
  where
    resultToSqlMapping
        :: (e :> es)
        => TypecheckerEnv e
        -> AliasedExpr TypeInfo
        -> Eff es SqlMapping
    resultToSqlMapping env = \case
        WithAlias expr alias -> do
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
        when ((exprType tyExpr).pgType /= PgBoolean) do
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
        when ((exprType tyExpr).pgType /= PgBoolean) do
            throw env.compileError $ ConditionTypeError tyExpr "JOIN/ON"
        return $ JcOn tyExpr

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
    (EGuardedBoolAnd{}) -> TypeInfo PgBoolean Nullable Nothing ''Bool
    (EGuardedMaybeAnd{}) -> TypeInfo PgBoolean Nullable Nothing ''Bool
    (ELit _ _ ty) -> ty
    (ECol _ _ _ ty) -> ty
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
    => TypecheckerEnv e
    -> Expr ()
    -> Eff es (Expr TypeInfo)
tcExpr env = \cases
    (EPgCast p1 var@(EParam pParam name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        let pgTy = getPgType env.database identifier
        let hsType = getHsType env.database pgTy
        introduceCommandInput env $
            CommandParameter $
                Parameter
                    { position = pParam
                    , pIdentifier = name
                    , paramType = SimpleParameter
                    , info =
                        Just
                            ParameterInfo
                                { pgType = pgTy
                                , hsType = hsType
                                , nullable = NonNullable
                                }
                    }
        let typeInfo = TypeInfo pgTy NonNullable (Just name.hsIdentifier) hsType
        let tyVar = EParam pParam name typeInfo
        return $ EPgCast p1 tyVar p2 identifier typeInfo
    (EPgCast p1 var@(EParamMaybe pParam name _) p2 identifier ()) -> do
        -- FIXME check if can be casted
        let pgTy = getPgType env.database identifier
        let hsTy = getHsType env.database pgTy
        introduceCommandInput env $
            CommandParameter $
                Parameter
                    { position = pParam
                    , pIdentifier = name
                    , paramType = SimpleMaybeParameter
                    , info =
                        Just
                            ParameterInfo
                                { pgType = pgTy
                                , hsType = hsTy
                                , nullable = Nullable
                                }
                    }
        let typeInfo = TypeInfo pgTy Nullable (Just name.hsIdentifier) hsTy
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
                let hsTy = getHsType env.database ty
                return $ EFunction p name tyExprs (TypeInfo ty resNullable Nothing hsTy)
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
    (ECol p mbAlias colName _) -> do
        fields <- get env.fields
        -- FIXME use alias
        case getColumnByName fields mbAlias colName of
            [] -> throw env.compileError $ ColumnDoesNotExist p mbAlias colName
            [envCol] -> do
                let hsTy = getHsType env.database envCol.typeName
                return $
                    ECol
                        p
                        mbAlias
                        colName
                        (TypeInfo envCol.typeName envCol.nullable (Just envCol.label) hsTy)
            ts -> throw env.compileError $ ColumnNameIsAmbiguous p colName
    expr@(ENot p innerExpr) -> do
        tyExpr <- tcExpr env innerExpr
        let ty = exprType tyExpr
        when (ty.pgType /= PgBoolean) do
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
        let (TypeInfo tyLhs nullableLhs _ _) = exprType tcLhs
        let (TypeInfo tyRhs nullableRhs _ _) = exprType tcRhs
        let nullableRes = tcNullable nullableLhs nullableRhs
        case getBinaryOpResult env.database tyLhs op tyRhs of
            Just tyRes -> do
                let hsTy = getHsType env.database tyRes
                return $ EBinOp p tcLhs op tcRhs (TypeInfo tyRes nullableRes Nothing hsTy)
            Nothing -> throw env.compileError $ OperatorDoesntExist p tyLhs op tyRhs
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
        when ((exprType tyLhs).pgType /= PgBoolean) do
            throw env.compileError $ ArgumentTypeError tyLhs func PgBoolean
        when ((exprType tyRhs).pgType /= PgBoolean) do
            throw env.compileError $ ArgumentTypeError tyRhs func PgBoolean
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
        binOpsMap = database.binaryOps
    listToMaybe
        . map (\(_, _, _, ty) -> ty)
        . filter (\(_, _, r, _) -> r == rhs)
        . filter (\(_, l, _, _) -> l == lhs)
        . filter (\(o, _, _, _) -> o == realOp)
        $ database.binaryOps

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
getPgType database identifier = find identifier database.typesMap
  where
    find :: Identifier -> [(Identifier, PgType, Name)] -> PgType
    find identifier = \cases
        -- TODO proper error
        [] -> error $ "no type: " <> show identifier
        ((i, t, _) : xs) ->
            if i == identifier
                then t
                else find identifier xs

getHsType :: Database -> PgType -> Name
getHsType database pgType = find pgType database.typesMap
  where
    find :: PgType -> [(Identifier, PgType, Name)] -> Name
    find identifier = \cases
        -- TODO proper error
        [] -> error $ "no type: " <> show identifier
        ((_, t, n) : xs) ->
            if t == pgType
                then n
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
