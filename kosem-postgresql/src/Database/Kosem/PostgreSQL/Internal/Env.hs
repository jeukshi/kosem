{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Env where

import Control.Applicative (Alternative)
import Control.Monad.Except (ExceptT, MonadError (throwError), runExceptT)
import Control.Monad.Identity (Identity (runIdentity), IdentityT (runIdentityT))
import Control.Monad.Reader (MonadReader (ask), ReaderT (runReaderT), asks)
import Control.Monad.State.Strict (MonadState (get, put), StateT (runStateT), evalStateT)
import Control.Monad.Trans (MonadTrans, lift)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Types

data IntroType
    = Subquery
    | Join
    deriving (Show)

data Field = Field
    { alias :: Text
    , label :: ColumnName
    , typeName :: PgType
    }
    deriving (Show)

data Env = Env
    { fields :: [Field]
    , params :: [Text]
    }

emptyEnv :: Env
emptyEnv = Env [] []

data TcError
    = NotInScope Text
    | AmbiguousColumnReference Text
    | Err Text
    deriving (Show)

type TcM = ReaderT Database (StateT Env (ExceptT TcError Identity))

newtype Tc a = Tc
    { runTc :: TcM a
    }
    deriving (Monad, Functor, Applicative, MonadTc)

runProgram :: Database -> Tc a -> Either TcError a
runProgram schema prog =
    runIdentity
        . runExceptT
        . flip evalStateT emptyEnv
        . flip runReaderT schema
        $ runTc prog

class (Monad m) => MonadTc m where
    getEnv :: m Env
    setEnv :: m Env
    getTableByName :: Text -> m [Table]
    getColumnByName :: ColumnName -> m [Field]
    addFieldsToEnv :: [Field] -> m ()
    getParamNumber :: Text -> m (Maybe Int)
    addParam :: Text -> m Int

    throwError :: TcError -> m a

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

    getTableByName :: Text -> TcM [Table]
    getTableByName tableName =
        asks (filter (\table -> table.name == tableName) . tables)

    getColumnByName :: ColumnName -> TcM [Field]
    getColumnByName name =
        filter (\e -> e.label == name)
            <$> fmap (.fields) get

    getParamNumber :: Text -> TcM (Maybe Int)
    getParamNumber name = do
        params <- fmap (.params) get
        case filter (\ixElem -> snd ixElem == name) (zip [1 ..] params) of
            [] -> return Nothing
            [(ix, _)] -> return $ Just ix
            _ -> error "oops" -- FIXME

    addParam :: Text -> TcM Int
    addParam name = do
        currentEnv <- get
        put (currentEnv{params = currentEnv.params ++ [name]})
        -- | + 1 for new elemen we just added.
        return $ length currentEnv.params + 1

    throwError :: TcError -> TcM a
    throwError = Control.Monad.Except.throwError
