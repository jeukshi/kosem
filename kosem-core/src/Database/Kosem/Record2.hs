{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Kosem.Record2 where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Data.List
import GHC.Exts (Any)
import GHC.Records
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

type (:=) :: Symbol -> Type -> Type
data label := ty = (KnownSymbol label) => Proxy label := ty

-- TODO instances?
-- TODO small array
newtype Row (types :: [Type]) = Row [Any]

instance HasField l (Row ((l := t) : e)) t where
    getField (Row xs) = unsafeCoerce $ xs !! 0
    {-# INLINE getField #-}

instance HasField l (Row (_0 : (l := t) : e)) t where
    getField (Row xs) = unsafeCoerce $ xs !! 1
    {-# INLINE getField #-}

instance HasField l (Row (_0 : _1 : (l := t) : e)) t where
    getField (Row xs) = unsafeCoerce $ xs !! 2
    {-# INLINE getField #-}

data X = X Int
    deriving (Show)

f :: IO ()
f = do
    let z = X 6
    let x =
            Row [unsafeCoerce "hello", unsafeCoerce "abs", unsafeCoerce z]
                :: Row '["abc" := String, "xyz" := String, "cdy" := X]
    let y =
            Row
                [unsafeCoerce "hello", unsafeCoerce Row [unsafeCoerce "abc", unsafeCoerce "cfg"], unsafeCoerce z]
                :: Row '["abc" := String, "xyz" := Row '["iii" := String, "yyy" := String], "cdy" := X]

    print x.abc
    print y.xyz.iii
    print y.xyz.yyy
    print x.cdy
    -- print x
    let zz = ff y
    func y

    print zz.abc
    pure ()

func :: (HasField "xyz" a (Row '["iii" := String, "yyy" := String]))
  => HasField "cdy" a X
  => a -> IO ()
func x = do
    print x.xyz.iii
    print x.cdy


ff :: (HasField "xyz" a (Row '["iii" := String, "yyy" := String]))
  => HasField "cdy" a X
  => a -> a
ff x = x
