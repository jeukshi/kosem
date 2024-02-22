{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Type where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Ast
import Database.Kosem.PostgreSQL.Internal.Env
import Database.Kosem.PostgreSQL.Internal.Parser
import Database.Kosem.PostgreSQL.Schema.Internal.Parser
import Text.Megaparsec (parseMaybe, parseTest)

-- typecheck :: STerm -> TTerm
-- typecheck = \cases
-- (Select cols table) -> undefined

-- ast = fromMaybe $ parseMaybe selectCore "select a2 from tab1 join tab2 on true"
ast = fromJust $ parseMaybe selectCore "select a, b as xx from tab1 join tab2 on true"

-- rr = runProgram schema (typecheck ast)

typecheck :: STerm () -> Tc (STerm SqlType)
typecheck = \cases
    (Select res (Just (From fromItem)) whereClause) -> do
        tyFromItem <- tcFromItem fromItem
        tcRes <- tcSelectExpr res
        return $ Select tcRes (Just (From tyFromItem)) (tcWhereClause whereClause)
    (Select res Nothing whereClause) -> do
        tcRes <- tcSelectExpr res
        return $ Select tcRes Nothing (tcWhereClause whereClause)

tcWhereClause :: Maybe (Where ()) -> Maybe (Where SqlType)
tcWhereClause = \cases
    Nothing -> Nothing
    (Just (Where expr)) -> undefined

tcSelectExpr
    :: NonEmpty (AliasedExpr ())
    -> Tc (NonEmpty (AliasedExpr SqlType))
tcSelectExpr = mapM tc
  where
    tc :: AliasedExpr () -> Tc (AliasedExpr SqlType)
    tc = \cases
        (WithAlias expr colAlias maybeAs) -> do
            tcExpr <- tcExpr expr
            return $ WithAlias tcExpr colAlias maybeAs
        (WithoutAlias expr) -> do
            tcExpr <- tcExpr expr
            return $ WithoutAlias tcExpr

tcFromItem :: FromItem () -> Tc (FromItem SqlType)
tcFromItem = \cases
    (FiTableName (TableName tableName)) -> do
        table <- tableByName tableName
        addTableToEnv table
        return $ FiTableName (TableName tableName)
    (FiJoin lhs joinType rhs joinCondition) -> do
        tyLhs <- tcFromItem lhs
        tyRhs <- tcFromItem rhs
        tyJoinCondition <- tcJoinCondition joinCondition
        return $ FiJoin tyLhs joinType tyRhs tyJoinCondition

tcJoinCondition :: JoinCondition () -> Tc (JoinCondition SqlType)
tcJoinCondition = \cases
    JcUsing -> return JcUsing
    (JcOn expr) -> do
        tyExpr <- tcExpr expr
        return $ JcOn tyExpr

tcExpr :: Expr () -> Tc (Expr SqlType)
tcExpr = \cases
    (ELit litVal _) -> case litVal of
        NumericLiteral -> return $ ELit litVal (Scalar "numeric")
        TextLiteral _ -> return $ ELit litVal (Scalar "text")
        (BoolLiteral _) -> return $ ELit litVal (Scalar "boolean")
    (ECol colName _) -> do
        envCol <- columnByName colName
        return $ ECol colName (Scalar envCol.typeName)
    (ENot not expr) ->
        ENot not <$> tcExpr expr
    (EAnd lhs and rhs) ->
        EAnd <$> tcExpr lhs <*> pure and <*> tcExpr rhs
    (EOr lhs or rhs) ->
        EOr <$> tcExpr lhs <*> pure or <*> tcExpr rhs
    (ELessThan lhs rhs) ->
        ELessThan <$> tcExpr lhs <*> tcExpr rhs
    (EGreaterThan lhs rhs) ->
        EGreaterThan <$> tcExpr lhs <*> tcExpr rhs
    (ELessThanOrEqualTo lhs rhs) ->
        ELessThanOrEqualTo <$> tcExpr lhs <*> tcExpr rhs
    (EGreaterThanOrEqualTo lhs rhs) ->
        EGreaterThanOrEqualTo <$> tcExpr lhs <*> tcExpr rhs
    (EEqual lhs rhs) ->
        EEqual <$> tcExpr lhs <*> tcExpr rhs
    (ENotEqual lhs style rhs) ->
        ENotEqual <$> tcExpr lhs <*> pure style <*> tcExpr rhs
    (EBetween lhs between rhs1 and rhs2) ->
        EBetween
            <$> tcExpr lhs
            <*> pure between
            <*> tcExpr rhs1
            <*> pure and
            <*> tcExpr rhs2
    (ENotBetween lhs not between rhs1 and rhs2) ->
        ENotBetween
            <$> tcExpr lhs
            <*> pure not
            <*> pure between
            <*> tcExpr rhs1
            <*> pure and
            <*> tcExpr rhs2


columnByName :: Text -> Tc EnvElem
columnByName name =
    getColumnByName name >>= \case
        [] -> throwError $ Err ("column does not exist: " <> name)
        [t] -> return t
        ts -> throwError $ Err ("Column name is ambigious: " <> name)

tableByName :: Text -> Tc Table
tableByName name =
    getTableByName name >>= \case
        [] -> throwError $ Err "relation does not exist"
        [t] -> return t
        ts -> throwError $ Err "Table name is ambigious"

addTableToEnv :: Table -> Tc ()
addTableToEnv table =
    addToEnv . map (toEnvElem table.name) . columns $ table
  where
    toEnvElem :: Text -> Column -> EnvElem
    toEnvElem alias column =
        EnvElem
            { alias = alias
            , label = column.name
            , typeName = column.typeName
            }
