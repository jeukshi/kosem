{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Sql.TH where

import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import Data.List (nub)
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
    :: [(Exp, [(Identifier, Bool)])]
    -> Q Exp
genPatternMatch = \cases
    [] -> error "can't be"
    [(exp, [])] -> return exp
    cs -> do
        let is = nub . map fst . head . map snd $ cs
            matchArgs = map (second (map snd)) cs
        matchArgsQ <- traverse genCases matchArgs
        return $ CaseE (genCaseArgs is) matchArgsQ
  where
    genCaseArgs :: [Identifier] -> Exp
    genCaseArgs = \cases
        [] -> error "can't be"
        [i] -> do
            let iName = mkName (identifierToString i)
            VarE iName
        (i1 : i2 : is) -> do
            let i1Name = mkName (identifierToString i1)
                i2Name = mkName (identifierToString i2)
                appE = AppE (VarE i1Name) (VarE i2Name)
            go appE is
          where
            go :: Exp -> [Identifier] -> Exp
            go exp [] = exp
            go exp (i : is) = do
                let iName = mkName (identifierToString i)
                    appE = AppE exp (VarE iName)
                go exp is
    genCases
        :: (Exp, [Bool])
        -> Q Match
    genCases (exp, matches) = do
        let first = head matches
            rest = tail matches
            restQ = map (\x -> ConP (if x then 'True else 'False) [] []) rest
        return $
            Match
                (ConP (if first then 'True else 'False) [] restQ)
                (NormalB exp)
                []

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
