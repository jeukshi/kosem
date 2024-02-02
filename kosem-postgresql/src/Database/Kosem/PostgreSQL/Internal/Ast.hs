module Database.Kosem.PostgreSQL.Internal.Ast where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

-- TODO make t TypeData
data STerm t
  = Select (NonEmpty (AliasedExpr t)) (From t)
  deriving (Show)

data FromItem t
  = FiTableName TableName
  | FiJoin (FromItem t) JoinType (FromItem t) (JoinCondition t)
  deriving (Show)

data From t = From (FromItem t)
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

data AliasedExpr t
  = WithAlias (Expr t) ColumnAlias (Maybe As)
  | WithoutAlias (Expr t)
  deriving (Show)

data SqlType
  = Scalar Text
  deriving (Show)
-- data TExpr
  -- = Typed Expr DbType


data Expr t
  = ELit LiteralValue t
  | ECol ColumnName t
  deriving (Show)

data LiteralValue
  = NumericLiteral
  | TextLiteral
  | BoolLiteral Text
  deriving (Show)

data TableName = TableName Text
  deriving (Show)
