{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Database.Kosem.PostgreSQL.Internal.Sql.TH where

import Control.Monad (join)
import Data.Bifunctor (Bifunctor (..), second)
import Data.ByteString (ByteString)
import Data.Int (Int8)
import Data.List (foldl', nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Vector.Fusion.Bundle.Monadic (elements)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.Sql.Types
import Database.Kosem.PostgreSQL.Internal.ToField (
    ToField (toField'Internal, toFieldWithLen'Internal),
 )
import Database.Kosem.PostgreSQL.Internal.Types
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax (Lift)
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
    genParseField sqlMapping = do
        let appTypeE = case sqlMapping.nullable of
                -- \| @Type
                NonNullable -> ConT sqlMapping.hsType
                -- \| @(Maybe Type)
                Nullable -> AppT (ConT ''Maybe) (ConT sqlMapping.hsType)
        let lenE = LitE (IntegerL (fromIntegral sqlMapping.len))
        -- \| `unsafeCoerce . parseField'Internal @Text`
        -- \| `unsafeCoerce . parseFieldWithLen'Internal @Text` <len>
        if sqlMapping.len > 0
            then
                InfixE
                    (Just (VarE 'unsafeCoerce))
                    (VarE '(.))
                    (Just (AppE (AppTypeE (VarE 'parseFieldWithLen'Internal) appTypeE) lenE))
            else
                InfixE
                    (Just (VarE 'unsafeCoerce))
                    (VarE '(.))
                    (Just (AppTypeE (VarE 'parseField'Internal) appTypeE))

hsIdentifierToVarE :: HsIdentifier -> Exp
hsIdentifierToVarE = \cases
    (MkHsIdentifier i []) ->
        VarE . mkName . identifierToString $ i
    (MkHsIdentifier i (firstId : restIds)) -> do
        let varE = VarE . mkName . identifierToString $ i
        let firstDot = GetFieldE varE (identifierToString firstId)
        let rest = map identifierToString restIds
        -- \| Fold into:
        -- GetFieldE (GetFieldE (VarE var) "inside") "outside"
        -- which is: `var.inside.outside`
        foldl' GetFieldE firstDot rest

{- |
Generate case expression selecting expression `EXP`.

For example:
```
case (x, y, z) of
    (True, (Just _), True) -> `EXP`
    (True, Nothing, False) -> `EXP`
    ...
```

This works under few assumptions:
* Every `EXP` is of the same type.
* Every `Choice` is different
* Every combination of True/False is present -
  patterns are exhaustive.
-}
genPatternMatch
    :: (Command Exp Exp -> Exp)
    -> CommandVariant Exp Exp
    -> Q Exp
genPatternMatch f = \cases
    (SingleCommand command) -> return $ f command
    (TwoCommands identifier choice1 cmd1 choice2 cmd2) -> do
        let idVarE = hsIdentifierToVarE identifier
        let matches =
                [ Match
                    (choiceOptionToPat choice1)
                    (NormalB (f cmd1))
                    []
                , Match
                    (choiceOptionToPat choice2)
                    (NormalB (f cmd2))
                    []
                ]
        return $ CaseE idVarE matches
    (MultipleCommands cmds) -> do
        let caseTuple =
                TupE
                    . map
                        ( Just
                            . hsIdentifierToVarE
                            . choiceIdentifier
                        )
                    . fst
                    . head
                    $ cmds
        let matchesTuple =
                map
                    ( (\(p, c) -> Match p (NormalB (f c)) [])
                        . first
                            ( TupP
                                . map (choiceOptionToPat . choiceOption)
                            )
                    )
                    cmds
        return $ CaseE caseTuple matchesTuple
  where
    choiceOptionToPat :: ChoiceOption -> Pat
    choiceOptionToPat = \cases
        (VcBool False) -> ConP 'False [] []
        (VcBool True) -> ConP 'True [] []
        (VcMaybe True) -> ConP 'Just [] [WildP]
        (VcMaybe False) -> ConP 'Nothing [] []

genCommand :: ByteString -> Q Exp
genCommand bs = [e|bs|]

genInt8 :: Int -> Q Exp
genInt8 int = [e|int|]

genParamsList :: [CommandParameter] -> Exp
genParamsList = \cases
    [] -> ConE '[]
    names -> ListE $ map genToField names
  where
    genToField :: CommandParameter -> Exp
    genToField param = do
        let idVarE = hsIdentifierToVarE param.cpIdentifier
        let appTypeE = case param.cpIsNullable of
                -- \| @Type
                NonNullable -> ConT param.cpHsType
                -- \| @(Maybe Type)
                Nullable -> AppT (ConT ''Maybe) (ConT param.cpHsType)
        let lenE = LitE (IntegerL (fromIntegral param.cpLen))
        if param.cpLen > 0
            then
                AppE
                    ( AppE
                        (AppTypeE (VarE 'toFieldWithLen'Internal) appTypeE)
                        lenE
                    )
                    idVarE
            else
                AppE
                    (AppTypeE (VarE 'toField'Internal) appTypeE)
                    idVarE
