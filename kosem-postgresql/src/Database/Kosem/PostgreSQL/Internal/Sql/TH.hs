{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Sql.TH where

import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.ToField (ToField (toField'Internal))
import Database.Kosem.PostgreSQL.Internal.Types
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Unsafe.Coerce (unsafeCoerce)

genRowType :: NonEmpty SqlMapping -> Q Type
genRowType columns = return $ AppT (ConT ''Row) (go $ NonEmpty.toList columns)
  where
    go = \cases
        (column : columns) -> AppT (makeTuple column) (go columns)
        [] -> PromotedNilT
    makeTuple sqlMapping = case sqlMapping.nullable of
        -- \| `label := type`
        NonNullable ->
            AppT
                PromotedConsT
                ( AppT
                    ( AppT
                        (ConT ''(:=))
                        (LitT (StrTyLit (identifierToString sqlMapping.identifier)))
                    )
                    (ConT sqlMapping.hsType)
                )
        -- \| `label := (Maybe type)`
        Nullable ->
            AppT
                PromotedConsT
                ( AppT
                    ( AppT
                        (ConT ''(:=))
                        (LitT (StrTyLit (identifierToString sqlMapping.identifier)))
                    )
                    (AppT (ConT ''Maybe) (ConT sqlMapping.hsType))
                )

genRowParser :: NonEmpty SqlMapping -> Q Exp
genRowParser sqlMappings =
    return
        . ListE
        . map genParseField
        $ NonEmpty.toList sqlMappings
  where
    genParseField :: SqlMapping -> Exp
    genParseField sqlMapping = case sqlMapping.nullable of
        NonNullable ->
            -- \| `unsafeCoerce . parseField'Internal @Text`
            InfixE
                (Just (VarE 'unsafeCoerce))
                (VarE '(.))
                (Just (AppTypeE (VarE 'parseField'Internal) (ConT sqlMapping.hsType)))
        Nullable ->
            -- \| `unsafeCoerce . parseField'Internal @(Maybe Text)`
            InfixE
                (Just (VarE 'unsafeCoerce))
                (VarE '(.))
                ( Just
                    ( AppTypeE
                        (VarE 'parseField'Internal)
                        (AppT (ConT ''Maybe) (ConT sqlMapping.hsType))
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
