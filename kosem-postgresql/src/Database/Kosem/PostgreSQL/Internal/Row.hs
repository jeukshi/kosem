{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}

module Database.Kosem.PostgreSQL.Internal.Row where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Database.PostgreSQL.LibPQ (Result)
import GHC.Exts (Any)
import GHC.Records
import GHC.TypeLits
import Language.Haskell.TH.Syntax (Lift (..))
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

instance HasField l (Row (_0 : _1 : _2 : (l := t) : e)) t where
    getField (Row xs) = unsafeCoerce $ xs !! 3
    {-# INLINE getField #-}

instance HasField l (Row (_0 : _1 : _2 : _3 : (l := t) : e)) t where
    getField (Row xs) = unsafeCoerce $ xs !! 4
    {-# INLINE getField #-}

class FromRow a where
    fromRow :: Result -> Int -> a

instance FromRow (Row '[l := t]) where
    fromRow res col = undefined
