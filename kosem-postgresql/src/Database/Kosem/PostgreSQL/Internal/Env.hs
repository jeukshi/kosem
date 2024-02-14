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
import Database.Kosem.PostgreSQL.Schema.Internal.Parser

data IntroType
    = Subquery
    | Join
    deriving (Show)

data EnvElem = EnvElem
    { alias :: Text
    , label :: ColumnName
    , typeName :: PgType
    }
    deriving (Show)

type Env = [EnvElem]

emptyEnv :: Env
emptyEnv = []




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
    getColumnByName :: Text -> m [EnvElem]
    xxx :: m Env
    addToEnv :: Env -> m ()

    throwError :: TcError -> m a

instance MonadTc TcM where
    getEnv = do
        x <- ask
        get
    setEnv :: TcM Env
    setEnv = undefined
    xxx = undefined

    addToEnv :: Env -> TcM ()
    addToEnv new = do
      curr <- get
      put (curr ++ new)
      return ()

    getTableByName :: Text -> TcM [Table]
    getTableByName tableName =
        asks (filter (\table -> table.name == tableName) . tables)

    getColumnByName :: Text -> TcM [EnvElem]
    getColumnByName name =
        filter (\e -> e.label == ColumnName name) <$> get

    throwError :: TcError -> TcM a
    throwError = Control.Monad.Except.throwError
