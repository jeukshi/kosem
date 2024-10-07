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
    = FiTableName P Identifier
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

data And = And
    deriving (Show)

data Or = Or
    deriving (Show)

data Not = Not
    deriving (Show)

data Between = Between
    deriving (Show)

data AliasedExpr t
    = WithAlias (Expr t) Identifier (Maybe As)
    | WithoutAlias (Expr t)
    deriving (Show)

data Expr t
    = EParens P (Expr t) P t
    | EParam P Identifier t
    | EParamMaybe P Identifier t
    | ELit P LiteralValue t
    | ECol P Identifier t -- TODO rename to identifier https://www.postgresql.org/docs/current/sql-syntax-lexical.html
    | EPgCast P (Expr t) P Identifier t -- TODO Identifi
    | -- | expression::type
      ENot P Not (Expr t)
    | EGuardedBoolAnd (Expr t) P Identifier P (Expr t) P
    | EGuardedMaybeAnd (Expr t) P Identifier P (Expr t) P
    | EAnd P (Expr t) And (Expr t)
    | EOr P (Expr t) Or (Expr t)
    | EBinOp P (Expr t) Operator (Expr t) t
    | -- Comparsion predicates
      EBetween P (Expr t) Between (Expr t) And (Expr t)
    | ENotBetween P (Expr t) Not Between (Expr t) And (Expr t)
    deriving (Show)

data LiteralValue
    = NumericLiteral
    | TextLiteral String
    | BoolLiteral String
    deriving (Show)
