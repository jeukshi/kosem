{-# LANGUAGE DataKinds #-}
{-# LANGUGAE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.TH where

import Data.Text (Text)
import Database.Kosem.PostgreSQL.Internal.FromField
import Database.Kosem.PostgreSQL.Internal.Row
import GHC.Records
import Language.Haskell.TH
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Unsafe.Coerce (unsafeCoerce)

data X = X Int
    deriving (Show)

f :: IO ()
f = do
    let z = X 6
    let x =
            Row [unsafeCoerce (Just ("hello" :: Text)), unsafeCoerce "abs", unsafeCoerce z]
                :: Row '["abc" := Maybe Text, "xyz" := String, "cdy" := X]

    let y =
            Row
                [unsafeCoerce "hello", unsafeCoerce Row [unsafeCoerce "abc", unsafeCoerce "cfg"], unsafeCoerce z]
                :: Row '["abc" := String, "xyz" := Row '["iii" := String, "yyy" := String], "cdy" := X]
    let row =
            Row [unsafeCoerce ("hello" :: Text), unsafeCoerce ("haskell" :: String)]
                :: Row '["abc" := Text, "xyz" := String]

    print row.abc
    print row.xyz

    case (row.abc, row.xyz) of
      ("abc", "yxz") -> print "das"
      _ -> print "no"
    -- let zzz = mk zz

    -- print zzz.abc
    pure ()

mk :: Row a -> Row a
mk _ = Row [unsafeCoerce ("dasd" :: Text), unsafeCoerce ("dsad" :: Text)]

type Xx = Row '["abc" := String, "xyz" := String, "cdy" := X]
x =
    Row [unsafeCoerce "hello", unsafeCoerce "abs", unsafeCoerce (X 5)]
        :: Row '["abc" := String, "xyz" := String, "cdy" := X]

func
    :: (HasField "xyz" a (Row '["iii" := String, "yyy" := String]))
    => (HasField "cdy" a X)
    => a
    -> IO ()
func x = do
    print x.xyz.iii
    print x.cdy

ff
    :: (HasField "xyz" a (Row '["iii" := String, "yyy" := String]))
    => (HasField "cdy" a X)
    => a
    -> a
ff x = x

genRowD :: Q [Dec]
genRowD = do
    reccc <- newName "myRec"
    {-let x =
            SigE
                (VarE reccc)
                ( AppT
                    (ConT ''Row)
                    ( AppT
                        (AppT PromotedConsT (AppT (AppT (ConT ''(:=)) (LitT (StrTyLit "abc"))) (ConT ''String)))
                        ( AppT
                            (AppT PromotedConsT (AppT (AppT (ConT ''(:=)) (LitT (StrTyLit "xyz"))) (ConT ''String)))
                            ( AppT
                                ( AppT
                                    PromotedConsT
                                    ( AppT
                                        (AppT (ConT ''(:=)) (LitT (StrTyLit "cdy")))
                                        (ConT ''X)
                                    )
                                )
                                PromotedNilT
                            )
                        )
                    )
                )-}
    let ty =
            SigD
                reccc
                ( AppT
                    (ConT ''Row)
                    ( AppT
                        (AppT PromotedConsT (AppT (AppT (ConT ''(:=)) (LitT (StrTyLit "abc"))) (ConT ''String)))
                        ( AppT
                            (AppT PromotedConsT (AppT (AppT (ConT ''(:=)) (LitT (StrTyLit "xyz"))) (ConT ''String)))
                            ( AppT
                                (AppT PromotedConsT (AppT (AppT (ConT ''(:=)) (LitT (StrTyLit "cdy"))) (ConT ''X)))
                                PromotedNilT
                            )
                        )
                    )
                )
    let val = ValD (VarP reccc) (NormalB (AppE (ConE ''Row) (ConE ''[]))) []
    return [ty, val]

genRowT' :: Q Type
genRowT' = do
    let x =
            ( AppT
                (ConT ''Row)
                ( AppT
                    (AppT PromotedConsT (AppT (AppT (ConT ''(:=)) (LitT (StrTyLit "abc"))) (ConT ''Text)))
                    ( AppT
                        (AppT PromotedConsT (AppT (AppT (ConT ''(:=)) (LitT (StrTyLit "xyz"))) (ConT ''Text)))
                        ( AppT
                            ( AppT PromotedConsT (AppT (AppT (ConT ''(:=)) (LitT (StrTyLit "cdy"))) (ConT ''Text))
                            )
                            PromotedNilT
                        )
                    )
                )
            )
    return x

genRowT :: [String] ->  Q Type
genRowT labels =  return $ AppT (ConT ''Row) (go labels)
  where
    go = \cases
      (label:labels) -> AppT (makeTuple label) (go labels)
      [] -> PromotedNilT

    -- | `label := type`
    makeTuple label = AppT PromotedConsT (AppT (AppT (ConT ''(:=)) (LitT (StrTyLit label))) (ConT ''Text))

{-rowType :: QuasiQuoter
rowType =
    QuasiQuoter
        { quotePat = error "quasiquoter used in pattern context"
        , quoteType = genRowT
        , quoteDec = error "quasiquoter used in declaration context"
        , quoteExp = error ""
        }-}

genRowParser :: Int -> Q Exp
genRowParser size =
    return
        $ ListE
        $ replicate
            size
            -- | `unsafeCoerce .parseField @Text`
            (InfixE (Just (VarE 'unsafeCoerce)) (VarE '(.)) (Just (AppTypeE (VarE 'parseField) (ConT ''Text))))

{-
  let x =
          ListE
              [ InfixE
                  (Just (VarE 'unsafeCoerce))
                  (VarE '(.))
                  ( Just
                      (AppTypeE (VarE 'parseField) (ConT ''Text))
                  )
              , InfixE
                  (Just (VarE 'unsafeCoerce))
                  (VarE '(.))
                  ( Just
                      (AppTypeE (VarE 'parseField) (ConT ''Text))
                  )
              ]
  return x
-}
