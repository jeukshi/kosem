{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Database.Kosem.PostgreSQL.Internal.Type where

import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Ast
import Database.Kosem.PostgreSQL.Internal.Env
import Database.Kosem.PostgreSQL.Internal.Parser
import Text.Megaparsec (parseMaybe, parseTest)

-- typecheck :: STerm -> TTerm
-- typecheck = \cases
-- (Select cols table) -> undefined

s =
    Schema
        { tables =
            [ Table
                { name = "tab1"
                , columns = [Column "a" "text", Column "b" "text"]
                }
            , Table
                { name = "tab2"
                , columns = [Column "a2" "text", Column "b2" "text"]
                }
            , Table
                { name = "tab3"
                , columns = [Column "a3" "text", Column "b3" "text"]
                }
            ]
        }

-- ast = fromMaybe $ parseMaybe selectCore "select a2 from tab1 join tab2 on true"
ast = fromJust $ parseMaybe selectCore "select a, b as xx from tab1 join tab2 on true"

rr = runProgram s (typecheck ast)

typecheck :: STerm () -> Tc (STerm SqlType)
typecheck = \cases
    (Select res (Just (From fromItem))) -> do
        tyFromItem <- tcFromItem fromItem
        tcRes <- tcSelectExpr res
        return $ Select tcRes (Just (From tyFromItem))
    (Select res Nothing) -> do
        tcRes <- tcSelectExpr res
        return $ Select tcRes Nothing


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
