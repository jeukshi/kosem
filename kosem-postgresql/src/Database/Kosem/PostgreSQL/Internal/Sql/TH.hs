{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Sql.TH where

import Control.Monad (join)
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text)
import Data.Vector.Fusion.Bundle.Monadic (elements)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Row
import Database.Kosem.PostgreSQL.Internal.Sql.Types
import Database.Kosem.PostgreSQL.Internal.ToField (ToField (toField'Internal))
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

{- |
Generate case expression selecting expression `EXP`.

For example:
```
case x, y, z of
    True True True -> `EXP`
    True True False -> `EXP`
    ...
```
In TH it looks like this:
```
CaseE
    (AppE
        (AppE (VarE x) (VarE y))
        (VarE z)
    )
    [ Match (ConP True []
         [ConP True [] [], ConP True [] []])
         (NormalB (`EXP`)) []
    , Match (ConP True []
         [ConP True [] [], ConP False [] []])
         (NormalB (`EXP`)) []
    , ...
    ]

```

This works under few assumptions:
* Every `EXP` is of the same type.
* Every list of `(Identifier, Bool)` is different
  only in the values of `Bool`.
* Every combination of True/False is present -
  patterns are exhaustive.
-}
genPatternMatch
    :: Either Exp (NonEmpty (Exp, NonEmpty Path))
    -> Q Exp
genPatternMatch = \cases
    (Left exp) -> return exp
    (Right es) -> do
        -- FIXME this needs some different representation
        -- as paths always come in pairs
        if NonEmpty.length es == 2
            then do
                let varName =
                        mkName
                            . identifierToString
                            . pathIdentifier
                            -- \| Assume `paths` has two elements
                            -- with the same `Identifier`.
                            . NonEmpty.head
                            . snd
                            . NonEmpty.head
                            $ es
                let (matches :: [Match]) =
                        NonEmpty.toList
                            . NonEmpty.map
                                ( \(exp, path) ->
                                    Match
                                        (pathOptionToPat . pathOption . NonEmpty.head $ path)
                                        (NormalB exp)
                                        []
                                )
                            $ es
                return $ CaseE (VarE varName) matches
            else do
                let caseTuple =
                        TupE
                            . map (Just . VarE . mkName . identifierToString . pathIdentifier)
                            . NonEmpty.toList
                            . snd
                            . NonEmpty.head
                            $ es
                -- TODO ok, fix this mess
                let matchesTuple =
                        map (\(e, p) -> Match p (NormalB e) [])
                            . map (second TupP)
                            . map (second (map (pathOptionToPat)))
                            . map (second (map pathOption))
                            . map (second NonEmpty.toList)
                            . NonEmpty.toList
                            $ es
                return $ CaseE caseTuple matchesTuple
  where
    pathOptionToPat :: PathOption -> Pat
    pathOptionToPat = \cases
        PoFalse -> ConP 'False [] []
        PoTrue -> ConP 'True [] []
        PoJust -> ConP 'Just [] [WildP]
        PoNothing -> ConP 'Nothing [] []

genCommand :: ByteString -> Q Exp
genCommand bs = [e|bs|]

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
