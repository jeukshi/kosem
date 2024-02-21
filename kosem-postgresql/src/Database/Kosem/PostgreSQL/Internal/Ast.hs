module Database.Kosem.PostgreSQL.Internal.Ast where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Schema.Internal.Parser (PgType(..))

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
  deriving(Show)

data And = And
  deriving(Show)

data Not = Not
  deriving(Show)

data Or = Or
  deriving(Show)

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
  | EAnd (Expr t) And (Expr t)
  | EOr (Expr t) Or (Expr t)
  | ENot Not (Expr t)
  deriving (Show)

data LiteralValue
  = NumericLiteral
  | TextLiteral Text
  | BoolLiteral Text
  deriving (Show)

data TableName = TableName Text
  deriving (Show)
