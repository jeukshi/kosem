module Database.Kosem.PostgreSQL.Internal.Ast where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Schema.Internal.Parser (PgType (..))

-- TODO make t TypeData
data STerm t
  = Select (NonEmpty (AliasedExpr t)) (Maybe (From t)) (Maybe (Where t))
  deriving (Show)

data FromItem t
  = FiTableName TableName
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

-- data ResultColumn
-- = RCExpr Expr
-- deriving (Show)

-- | https://www.postgresql.org/docs/current/sql-expressions.html
type ColumnName = Text

type ColumnAlias = Text

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

data NotEqualStyle
  = NotEqualStandardStyle -- !=
  | NotEqualNonStandardStyle -- <>
  deriving (Show)

data AliasedExpr t
  = WithAlias (Expr t) ColumnAlias (Maybe As)
  | WithoutAlias (Expr t)
  deriving (Show)

data SqlType
  = Scalar PgType
  deriving (Show)

-- data TExpr
-- = Typed Expr DbType

data Expr t
  = ELit LiteralValue t
  | ECol ColumnName t -- TODO rename to identifier https://www.postgresql.org/docs/current/sql-syntax-lexical.html
  | EPgCast (Expr t) Text -- | expression::type
  | ENot Not (Expr t)
  | EAnd (Expr t) And (Expr t)
  | EOr (Expr t) Or (Expr t)
  -- Comparsion operators
  | ELessThan (Expr t) (Expr t)
  | EGreaterThan (Expr t) (Expr t)
  | ELessThanOrEqualTo (Expr t) (Expr t)
  | EGreaterThanOrEqualTo (Expr t) (Expr t)
  | EEqual (Expr t) (Expr t)
  | ENotEqual (Expr t) NotEqualStyle (Expr t)
  -- Comparsion predicates
  | EBetween (Expr t) Between (Expr t) And (Expr t)
  | ENotBetween (Expr t) Not Between (Expr t) And (Expr t)
  deriving (Show)

data LiteralValue
  = NumericLiteral
  | TextLiteral Text
  | BoolLiteral Text
  deriving (Show)

data TableName = TableName Text
  deriving (Show)
