{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.TH where

import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.Ast (IsNullable (..))
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.ToField (ToField (toField'Internal))
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Unsafe.Coerce (unsafeCoerce)

genRowType :: [(String, Name, IsNullable)] -> Q Type
genRowType columns = return $ AppT (ConT ''Row) (go columns)
  where
    go = \cases
        (column : columns) -> AppT (makeTuple column) (go columns)
        [] -> PromotedNilT
    -- \| `label := type`
    makeTuple = \cases
        (label, ty, NonNullable) ->
            AppT
                PromotedConsT
                ( AppT
                    (AppT (ConT ''(:=)) (LitT (StrTyLit label)))
                    (ConT ty)
                )
    -- \| `label := (Maybe type)`
        (label, ty, Nullable) ->
            AppT
                PromotedConsT
                ( AppT
                    (AppT (ConT ''(:=)) (LitT (StrTyLit label)))
                    (AppT (ConT ''Maybe) (ConT ty))
                )

genRowParser :: [(String, Name, IsNullable)] -> Q Exp
genRowParser names =
    return
        . ListE
        $ map genParseField names
  where
    genParseField :: (String, Name, IsNullable) -> Exp
    genParseField = \cases
        (_, ty, NonNullable) ->
            -- \| `unsafeCoerce . parseField'Internal @Text`
            InfixE
                (Just (VarE 'unsafeCoerce))
                (VarE '(.))
                (Just (AppTypeE (VarE 'parseField'Internal) (ConT ty)))
        (_, ty, Nullable) ->
            -- \| `unsafeCoerce . parseField'Internal @(Maybe Text)`
            InfixE
                (Just (VarE 'unsafeCoerce))
                (VarE '(.))
                ( Just
                    ( AppTypeE
                        (VarE 'parseField'Internal)
                        (AppT (ConT ''Maybe) (ConT ty))
                    )
                )

genParamsList :: [(String, Name, IsNullable)] -> Q Exp
genParamsList = \cases
    [] -> return $ ConE '[]
    names ->
        return $
            ListE $
                map genToField names
  where
    genToField :: (String, Name, IsNullable) -> Exp
    genToField = \cases
        (name, ty, NonNullable) -> do
            let hsName = mkName name
            -- \| toField'Internal @Type variable
            AppE
                (AppTypeE (VarE 'toField'Internal) (ConT ty))
                (VarE hsName)
        (name, ty, Nullable) -> do
            let hsName = mkName name
            -- \| toField'Internal @(Maybe Type) variable
            AppE
                ( AppTypeE
                    (VarE 'toField'Internal)
                    (AppT (ConT ''Maybe) (ConT ty))
                )
                (VarE hsName)
