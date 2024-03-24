{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Sql.Env where

import Control.Applicative (Alternative)
import Control.Monad (when)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity), IdentityT (runIdentityT))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State (gets)
import Control.Monad.State.Strict (MonadState (get, put), StateT (runStateT), evalStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.List (foldl')
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Diagnostics (CompileError)
import Database.Kosem.PostgreSQL.Internal.PgBuiltin (DatabaseConfig (binaryOperators))
import Database.Kosem.PostgreSQL.Internal.Sql.Types (Parameter (..), ParameterType (..))
import Database.Kosem.PostgreSQL.Internal.Types
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

data Env = Env
    { fields :: [Field]
    , params :: [Parameter]
    }

emptyEnv :: Env
emptyEnv = Env [] []

type TcM = ReaderT Database (StateT Env (ExceptT CompileError Identity))

newtype Tc a = Tc
    { runTc :: TcM a
    }
    deriving (Monad, Functor, Applicative, MonadTc)

runProgram :: Database -> Tc a -> Either CompileError a
runProgram schema prog =
    runIdentity
        . runExceptT
        . flip evalStateT emptyEnv
        . flip runReaderT schema
        $ runTc prog

class (Monad m) => MonadTc m where
    getEnv :: m Env
    setEnv :: m Env
    getType :: Identifier -> m PgType
    getHsType :: PgType -> m Name
    getBinaryOpResult :: PgType -> Operator -> PgType -> m (Maybe PgType)
    getTableByName :: Identifier -> m [Table]
    getColumnByName :: Identifier -> m [Field]
    addFieldsToEnv :: [Field] -> m ()
    introduceParameter
        :: Identifier -> PgType -> Name -> ParameterType -> m Int

    throwError :: CompileError -> m a

instance MonadTc TcM where
    getEnv = do
        x <- ask
        get
    setEnv :: TcM Env
    setEnv = undefined

    addFieldsToEnv :: [Field] -> TcM ()
    addFieldsToEnv newFields = do
        currentEnv <- get
        put (currentEnv{fields = currentEnv.fields ++ newFields})
        return ()

    getTableByName :: Identifier -> TcM [Table]
    getTableByName tableName =
        asks (filter (\table -> table.name == tableName) . tables)

    getColumnByName :: Identifier -> TcM [Field]
    getColumnByName name =
        filter (\e -> e.label == name)
            <$> fmap (.fields) get

    throwError :: CompileError -> TcM a
    throwError = Control.Monad.Except.throwError

    getType :: Identifier -> TcM PgType
    getType identifier = do
        types <- asks (.typesMap)
        pgType <- find identifier types
        pure pgType
      where
        find :: Identifier -> [(Identifier, PgType, Name)] -> TcM PgType
        find identifier = \cases
            [] -> error $ "no type: " <> show identifier
            ((i, t, _) : xs) ->
                if i == identifier
                    then pure t
                    else find identifier xs

    getHsType :: PgType -> TcM Name
    getHsType pgType = do
        types <- asks (.typesMap)
        name <- find pgType types
        pure name
      where
        find :: PgType -> [(Identifier, PgType, Name)] -> TcM Name
        find identifier = \cases
            [] -> error $ "no type: " <> show identifier
            ((_, t, n) : xs) ->
                if t == pgType
                    then pure n
                    else find identifier xs

    getBinaryOpResult :: PgType -> Operator -> PgType -> TcM (Maybe PgType)
    getBinaryOpResult lhs op rhs = do
        -- \| Postgres converts '!=' to '<>', see note:
        -- https://www.postgresql.org/docs/current/functions-comparison.html
        let realOp = if op == "!=" then "<>" else op
        binOpsMap <- asks (.binaryOps)
        return
            $ listToMaybe
                . map (\(_, _, _, ty) -> ty)
                . filter (\(_, _, r, _) -> r == rhs)
                . filter (\(_, l, _, _) -> l == lhs)
                . filter (\(o, _, _, _) -> o == realOp)
            $ binOpsMap

    introduceParameter
        :: Identifier -> PgType -> Name -> ParameterType -> TcM Int
    introduceParameter identifier pgType hsType paramType = do
        state <- getEnv
        let currentParams = state.params
        case filter (\x -> x.identifier == identifier) currentParams of
            [] -> do
                let newNumber = getMaxParamNumber currentParams + 1
                addParam newNumber identifier pgType hsType paramType
                return newNumber
            param : _ -> do
                when (param.pgType /= pgType) do
                    error "TODO"
                when (param.hsType /= hsType) do
                    error "TODO"
                when (param.paramType /= paramType) do
                    error "TODO"
                addParam param.number identifier param.pgType param.hsType param.paramType
                return param.number
      where
        getMaxParamNumber :: [Parameter] -> Int
        getMaxParamNumber = foldl' max 0 . map (.number)
        addParam number identifier pgType hsType paramType = do
            state <- getEnv
            let newParam =
                    Parameter
                        { number = number
                        , identifier = identifier
                        , pgType = pgType
                        , hsType = hsType
                        , paramType = paramType
                        }
            put state{params = state.params ++ [newParam]}
