{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExistentialQuantification #-}

module Database.Kosem.Record where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import GHC.Records
import GHC.TypeLits

data Rec2 a b = Rec2 a b

type (:=) :: Symbol -> Type -> Type
data label := ty = (KnownSymbol label) => Proxy label := ty

instance HasField label (Rec2 (label := ty) f1) ty where
  getField (Rec2 (_ := x) _1) = x

instance HasField label (Rec2 f0 (label := ty)) ty where
  getField :: Rec2 f0 (label := ty) -> ty
  getField (Rec2 _0 (_ := x)) = x

main :: IO ()
main = do
  let rr = Rec2 ((Proxy :: Proxy "sss") := "sss") ((Proxy :: Proxy "ds") := 123)
  print $ rr.ds
