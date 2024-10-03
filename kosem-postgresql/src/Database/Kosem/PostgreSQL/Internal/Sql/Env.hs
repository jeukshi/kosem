{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Sql.Env where

import Bluefin.Compound
import Bluefin.Eff
import Bluefin.Exception (Exception, throw, try)
import Bluefin.State
import Control.Applicative (Alternative)
import Control.Monad (when)
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Diagnostics (CompileError)
import Database.Kosem.PostgreSQL.Internal.P (P)
import Database.Kosem.PostgreSQL.Internal.PgBuiltin (DatabaseConfig (binaryOperators))
import Database.Kosem.PostgreSQL.Internal.Sql.Types (
    CommandInput,
    Parameter (..),
    ParameterType (..),
 )
import Database.Kosem.PostgreSQL.Internal.Types
import GHC.Records (HasField)
import Language.Haskell.TH (Name)

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

data EnvE e = MkEnvE
    { database :: Database
    , fields :: State [Field] e
    , commandInput :: State [CommandInput] e
    , compileError :: Exception CompileError e
    }

runEnv
    :: (e1 :> es)
    => Database
    -> Exception CompileError e1
    -> [Field]
    -> [CommandInput]
    -> (forall e. EnvE e -> Eff (e :& es) r)
    -> Eff es r
runEnv database ex fields cInput action =
    evalState fields $ \fieldsS -> do
        evalState cInput $ \cInputS -> do
            useImplIn
                action
                MkEnvE
                    { database = database
                    , fields = mapHandle fieldsS
                    , commandInput = mapHandle cInputS
                    , compileError = mapHandle ex
                    }

introduceCommandInput
    :: (e :> es)
    => EnvE e
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

getPgType :: Database -> Identifier -> PgType
getPgType database identifier = find identifier database.typesMap
  where
    find :: Identifier -> [(Identifier, PgType, Name)] -> PgType
    find identifier = \cases
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

getColumnByName :: [Field] -> Identifier -> [Field]
getColumnByName fields name =
    filter (\e -> e.label == name) fields
