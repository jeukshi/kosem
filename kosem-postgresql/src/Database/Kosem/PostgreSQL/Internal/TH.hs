{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.TH where

import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.ToField (ToField (toField'Internal))
import Database.Kosem.PostgreSQL.Internal.Types
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Unsafe.Coerce (unsafeCoerce)

genRowType :: [(Identifier, Name, IsNullable)] -> Q Type
genRowType columns = return $ AppT (ConT ''Row) (go columns)
  where
    go = \cases
        (column : columns) -> AppT (makeTuple column) (go columns)
        [] -> PromotedNilT
    makeTuple = \cases
        -- \| `label := type`
        (identifier, ty, NonNullable) ->
            AppT
                PromotedConsT
                ( AppT
                    ( AppT
                        (ConT ''(:=))
                        (LitT (StrTyLit (identifierToString identifier)))
                    )
                    (ConT ty)
                )
        -- \| `label := (Maybe type)`
        (identifier, ty, Nullable) ->
            AppT
                PromotedConsT
                ( AppT
                    ( AppT
                        (ConT ''(:=))
                        (LitT (StrTyLit (identifierToString identifier)))
                    )
                    (AppT (ConT ''Maybe) (ConT ty))
                )

genRowParser :: [(Identifier, Name, IsNullable)] -> Q Exp
genRowParser names =
    return
        . ListE
        $ map genParseField names
  where
    genParseField :: (Identifier, Name, IsNullable) -> Exp
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

genParamsList :: [(Identifier, Name, IsNullable)] -> Q Exp
genParamsList = \cases
    [] -> return $ ConE '[]
    names ->
        return $
            ListE $
                map genToField names
  where
    genToField :: (Identifier, Name, IsNullable) -> Exp
    genToField = \cases
        (name, ty, NonNullable) -> do
            let hsName = mkName (identifierToString name)
            -- \| toField'Internal @Type variable
            AppE
                (AppTypeE (VarE 'toField'Internal) (ConT ty))
                (VarE hsName)
        (name, ty, Nullable) -> do
            let hsName = mkName (identifierToString name)
            -- \| toField'Internal @(Maybe Type) variable
            AppE
                ( AppTypeE
                    (VarE 'toField'Internal)
                    (AppT (ConT ''Maybe) (ConT ty))
                )
                (VarE hsName)
