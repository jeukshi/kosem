module Database.Kosem.PostgreSQL.Internal.Ast where

import Data.ByteString (ByteString)
import Data.ByteString.Builder
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (foldl')
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE
import Data.Text (Text)
import Data.Text qualified as T
import Database.Kosem.PostgreSQL.Internal.Classes
import Database.Kosem.PostgreSQL.Internal.PgBuiltin
import Database.Kosem.PostgreSQL.Internal.Types
import Database.Kosem.PostgreSQL.Internal.P (P)

astToRawSql :: STerm TypeInfo -> ByteString
astToRawSql = toStrict . toLazyByteString . toRawSql

(<->) :: Builder -> Builder -> Builder
(<->) lhs rhs = lhs <> " " <> rhs

-- TODO make t TypeData
data STerm t
  = Select (NonEmpty (AliasedExpr t)) (Maybe (From t)) (Maybe (Where t))
  deriving (Show)

collectExprs :: STerm TypeInfo -> [Expr TypeInfo]
collectExprs = \cases
  (Select output mbFrom mbWhere) ->
    fromOutput output
      ++ maybe [] fromFrom mbFrom
      ++ maybe [] fromWhere mbWhere
 where
  fromOutput :: NonEmpty (AliasedExpr TypeInfo) -> [Expr TypeInfo]
  fromOutput =
    map
      ( \case
          WithoutAlias expr -> expr
          WithAlias expr _ _ -> expr
      )
      . NE.toList
  fromWhere :: Where TypeInfo -> [Expr TypeInfo]
  fromWhere (Where expr) = [expr]
  fromFrom :: From TypeInfo -> [Expr TypeInfo]
  fromFrom (From fromItem) = fromFromItem fromItem
  fromFromItem :: FromItem TypeInfo -> [Expr TypeInfo]
  fromFromItem = \cases
    (FiTableName _ _) -> []
    (FiJoin lhs _ rhs joinCondition) ->
      fromJoinCondition joinCondition
        ++ fromFromItem lhs
        ++ fromFromItem rhs
  fromJoinCondition :: JoinCondition TypeInfo -> [Expr TypeInfo]
  fromJoinCondition = \cases
    JcUsing -> []
    (JcOn expr) -> [expr]

instance ToRawSql (STerm TypeInfo) where
  toRawSql = \cases
    (Select output from whereClause) ->
      "SELECT"
        <-> foldl' (<->) "" (NE.intersperse "," . NE.map toRawSql $ output)
        <-> maybe "" toRawSql from
        <-> maybe "" toRawSql whereClause

data FromItem t
  = FiTableName P Identifier
  | FiJoin (FromItem t) JoinType (FromItem t) (JoinCondition t)
  deriving (Show)

instance ToRawSql (FromItem TypeInfo) where
  toRawSql = \cases
    (FiTableName _ tableName) -> toRawSql tableName
    (FiJoin lhs joinType rhs condition) ->
      toRawSql lhs
        <-> toRawSql joinType
        <-> toRawSql rhs
        <-> toRawSql condition

data From t = From (FromItem t)
  deriving (Show)

instance ToRawSql (From TypeInfo) where
  toRawSql (From item) = "FROM" <-> toRawSql item

data Where t = Where (Expr t)
  deriving (Show)

instance ToRawSql (Where TypeInfo) where
  toRawSql :: Where TypeInfo -> Builder
  toRawSql (Where expr) = "WHERE" <-> toRawSql expr

data JoinCondition t
  = JcOn (Expr t)
  | JcUsing
  deriving (Show)

instance ToRawSql (JoinCondition TypeInfo) where
  toRawSql :: JoinCondition TypeInfo -> Builder
  toRawSql = \cases
    (JcOn expr) -> "ON" <-> toRawSql expr
    JcUsing -> undefined

data JoinType
  = JtJoin
  | JtLeftJoin
  deriving (Show)

instance ToRawSql JoinType where
  toRawSql :: JoinType -> Builder
  toRawSql = \cases
    JtJoin -> "JOIN"
    JtLeftJoin -> "LEFT JOIN"

data Inner = Inner
  deriving (Show)

instance ToRawSql Inner where
  toRawSql :: Inner -> Builder
  toRawSql _ = "INNER"

data Outer = Outer
  deriving (Show)

instance ToRawSql Outer where
  toRawSql :: Outer -> Builder
  toRawSql _ = "OUTER"

data As = As
  deriving (Show)

instance ToRawSql As where
  toRawSql :: As -> Builder
  toRawSql _ = "AS"

data And = And
  deriving (Show)

instance ToRawSql And where
  toRawSql :: And -> Builder
  toRawSql _ = "AND"

data Or = Or
  deriving (Show)

instance ToRawSql Or where
  toRawSql :: Or -> Builder
  toRawSql _ = "OR"

data Not = Not
  deriving (Show)

instance ToRawSql Not where
  toRawSql :: Not -> Builder
  toRawSql _ = "NOT"

data Between = Between
  deriving (Show)

instance ToRawSql Between where
  toRawSql :: Between -> Builder
  toRawSql _ = "BETWEEN"

data AliasedExpr t
  = WithAlias (Expr t) Identifier (Maybe As)
  | WithoutAlias (Expr t)
  deriving (Show)

instance ToRawSql (AliasedExpr TypeInfo) where
  toRawSql :: AliasedExpr TypeInfo -> Builder
  toRawSql = \cases
    (WithoutAlias expr) -> toRawSql expr
    (WithAlias expr alias Nothing) ->
      toRawSql expr <-> toRawSql alias
    (WithAlias expr alias (Just as)) ->
      toRawSql expr <-> toRawSql as <-> toRawSql alias

-- | Equals, ignoring IsNullable
(~==~) :: TypeInfo -> TypeInfo -> Bool
(~==~) = \cases
  (TypeInfo lhs _) (TypeInfo rhs _) -> lhs == rhs

-- | Not equals, ignoring IsNullable
(~/=~) :: TypeInfo -> TypeInfo -> Bool
(~/=~) = \cases
  (TypeInfo lhs _) (TypeInfo rhs _) -> lhs /= rhs

isNullable :: TypeInfo -> IsNullable
isNullable = \cases
  (TypeInfo _ nullable) -> nullable

instance ToRawSql TypeInfo where
  toRawSql _ = ""

data Expr t
  = EParens P (Expr t) P t
  | EParam P Int Identifier t
  | EParamMaybe P Int Identifier t
  | ELit P LiteralValue t
  | ECol P Identifier t -- TODO rename to identifier https://www.postgresql.org/docs/current/sql-syntax-lexical.html
  | EPgCast P (Expr t) P Identifier t -- TODO Identifi
  | -- | expression::type
    ENot P Not (Expr t)
  | EAnd P (Expr t) And (Expr t)
  | EOr P (Expr t) Or (Expr t)
  | EBinOp P (Expr t) Operator (Expr t) t
  | -- Comparsion predicates
    EBetween P (Expr t) Between (Expr t) And (Expr t)
  | ENotBetween P (Expr t) Not Between (Expr t) And (Expr t)
  deriving (Show)

collectAllVariables :: STerm TypeInfo -> [(Int, Identifier, TypeInfo)]
collectAllVariables = concatMap collectVariables . collectExprs
 where
  collectVariables :: Expr TypeInfo -> [(Int, Identifier, TypeInfo)]
  collectVariables expr = go expr []
   where
    go :: Expr TypeInfo -> [(Int, Identifier, TypeInfo)] -> [(Int, Identifier, TypeInfo)]
    go expr acc = case expr of
      (EParam _ n t ty) -> acc ++ [(n, t, ty)]
      (EParamMaybe _ n t ty) -> acc ++ [(n, t, ty)]
      (EParens _ expr _ _) -> go expr acc
      (ELit _ lit _) -> acc
      (ECol _ columnName _) -> acc
      (EPgCast _ expr _ _ _) -> go expr acc
      (ENot _ not expr) -> go expr acc
      (EAnd _ lhs and rhs) -> go rhs (go lhs acc)
      (EOr _ lhs or rhs) -> go rhs (go lhs acc)
      (EBinOp _ lhs or rhs _) -> go rhs (go lhs acc)
      (EBetween _ lhs between rhs1 and rhs2) ->
        go rhs1 (go rhs2 (go lhs acc))
      (ENotBetween _ lhs not between rhs1 and rhs2) ->
        go rhs1 (go rhs2 (go lhs acc))

instance ToRawSql (Expr TypeInfo) where
  toRawSql :: Expr TypeInfo -> Builder
  toRawSql = \cases
    (EParam _ n _ _) -> textToBuilder (T.pack $ "$" <> show n)
    (EParamMaybe _ n _ _) -> textToBuilder (T.pack $ "$" <> show n)
    (EParens _ expr _ _) -> "(" <> toRawSql expr <> ")"
    (ELit _ lit _) -> toRawSql lit
    (ECol _ columnName _) -> toRawSql columnName
    (EPgCast _ lhs _ identifier _) -> toRawSql lhs <> "::" <> toRawSql identifier
    (ENot _ not rhs) -> toRawSql not <-> toRawSql rhs
    (EAnd _ lhs and rhs) -> toRawSql lhs <-> toRawSql and <-> toRawSql rhs
    (EOr _ lhs or rhs) -> toRawSql lhs <-> toRawSql or <-> toRawSql rhs
    (EBinOp _ lhs operator rhs _) ->
      toRawSql lhs <-> toRawSql operator <-> toRawSql rhs
    (EBetween _ lhs between rhs1 and rhs2) ->
      toRawSql lhs
        <-> toRawSql between
        <-> toRawSql rhs1
        <-> toRawSql and
        <-> toRawSql rhs2
    (ENotBetween _ lhs not between rhs1 and rhs2) ->
      toRawSql lhs
        <-> toRawSql not
        <-> toRawSql between
        <-> toRawSql rhs1
        <-> toRawSql and
        <-> toRawSql rhs2

data LiteralValue
  = NumericLiteral
  | TextLiteral Text
  | BoolLiteral Text
  deriving (Show)

instance ToRawSql LiteralValue where
  toRawSql :: LiteralValue -> Builder
  toRawSql = \cases
    NumericLiteral -> undefined
    (TextLiteral text) -> "'" <> textToBuilder text <> "'"
    (BoolLiteral text) -> textToBuilder text
