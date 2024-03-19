{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Sql.Env where

import Control.Applicative (Alternative)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity), IdentityT (runIdentityT))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State.Strict (MonadState (get, put), StateT (runStateT), evalStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Diagnostics (CompileError)
import Database.Kosem.PostgreSQL.Internal.PgBuiltin (DatabaseConfig (binaryOperators))
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
    , params :: [Identifier]
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
    getBinaryOpResult :: PgType -> Operator -> PgType -> m (Maybe PgType)
    getTableByName :: Identifier -> m [Table]
    getColumnByName :: Identifier -> m [Field]
    addFieldsToEnv :: [Field] -> m ()
    getParamNumber :: Identifier -> m (Maybe Int)
    addParam :: Identifier -> m Int

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

    getParamNumber :: Identifier -> TcM (Maybe Int)
    getParamNumber name = do
        params <- fmap (.params) get
        case filter (\ixElem -> snd ixElem == name) (zip [1 ..] params) of
            [] -> return Nothing
            [(ix, _)] -> return $ Just ix
            _ -> error "oops" -- FIXME

    addParam :: Identifier -> TcM Int
    addParam name = do
        currentEnv <- get
        put (currentEnv{params = currentEnv.params ++ [name]})
        -- \| + 1 for new elemen we just added.
        return $ length currentEnv.params + 1

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
