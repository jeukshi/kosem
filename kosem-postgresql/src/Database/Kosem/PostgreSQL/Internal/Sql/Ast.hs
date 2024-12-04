module Database.Kosem.PostgreSQL.Internal.Sql.Ast where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.P (P)
import Database.Kosem.PostgreSQL.Internal.PgBuiltin
import Database.Kosem.PostgreSQL.Internal.Types

-- TODO make t TypeData
data STerm t
    = Select (NonEmpty (AliasedExpr t)) (Maybe (From t)) (Maybe (Where t))
    deriving (Show)

data FromItem t
    = FiTableName P Identifier (Maybe Alias)
    | FiJoin (FromItem t) JoinType (FromItem t) (JoinCondition t)
    deriving (Show)

data From t = From (FromItem t)
    deriving (Show)

data Where t = Where (Expr t)
    deriving (Show)

data JoinCondition t
    = JcOn (Expr t)
    | JcUsing
    deriving (Show)

data JoinType
    = JtJoin
    | JtLeftJoin
    deriving (Show)

data Inner = Inner
    deriving (Show)

data Outer = Outer
    deriving (Show)

data As = As
    deriving (Show)

data AliasedExpr t
    = WithAlias (Expr t) Identifier (Maybe As)
    | WithoutAlias (Expr t)
    deriving (Show)

data Expr t
    = EParens P (Expr t) P t
    | EParam P HsIdentifier t
    | EParamMaybe P HsIdentifier t
    | EFunction P Identifier [Expr t] t
    | ELit P LiteralValue t
    | ECol P (Maybe Alias) Identifier t
    | EPgCast P (Expr t) P Identifier t
    | -- | expression::type
      ENot P (Expr t)
    | EGuardedBoolAnd (Expr t) P HsIdentifier P (Expr t) P
    | EGuardedMaybeAnd (Expr t) P HsIdentifier P (Expr t) P
    | EAnd P (Expr t) (Expr t)
    | EOr P (Expr t) (Expr t)
    | EBinOp P (Expr t) Operator (Expr t) t
    | -- Comparison predicates
      EBetween P (Expr t) (Expr t) (Expr t)
    | ENotBetween P (Expr t) (Expr t) (Expr t)
    deriving (Show)

data LiteralValue
    = NumericLiteral
    | TextLiteral String
    | BoolLiteral String
    deriving (Show)
