{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Database.Kosem.PostgreSQL.Internal.Row where

import Data.Data (Proxy (Proxy))
import Data.Kind (Type)
import Database.PostgreSQL.LibPQ (Result)
import GHC.Exts (Any)
import GHC.Records
import GHC.TypeLits
    ( KnownNat, KnownSymbol, Symbol, type (+), Nat, symbolVal )
import Language.Haskell.TH.Syntax (Lift (..))
import Unsafe.Coerce (unsafeCoerce)
import GHC.TypeNats (KnownNat, Nat, Natural, natVal, type (+), type (-))
import GHC.TypeError (TypeError, ErrorMessage (..))

type data Pair a b = a := b

-- TODO instances?
-- TODO small array
newtype Row (types :: [Pair Symbol Type]) = Row [Any]

type family Add1 (n :: Nat) :: Nat where
    Add1 n = n + 1

class Increment (n :: Nat) (m :: Nat) | n -> m

instance (KnownNat n, KnownNat m, m ~ Add1 n) => Increment n m

type family IsSameSymbol (a :: Symbol) (b :: Symbol) :: Bool where
    IsSameSymbol a a = 'True
    IsSameSymbol a b = 'False

-- TODO, maybe this could be simpler? Maybe Undecidable from GHC 9.8 would help?
class KeyNotIn (l :: Symbol) (xs :: [Pair Symbol Type])

instance KeyNotIn l '[]

instance
    ( KeyNotInStep (IsSameSymbol l l') l xs
    , KeyNotIn l xs
    ) =>
    KeyNotIn l ((l' := x) : xs)

class KeyNotInStep (isDup :: Bool) (l :: Symbol) (xs :: [Pair Symbol Type])

instance
    ( TypeError
        ( 'Text "Duplicate field label: "
            ':<>: 'ShowType l
        )
    ) =>
    KeyNotInStep 'True l xs

instance KeyNotInStep 'False l xs

class RowHasField (l :: Symbol) (types :: [Pair Symbol Type]) (t :: Type) (n :: Nat) | l types -> t

instance (KeyNotIn l rest, KnownNat n, Increment 0 n) => RowHasField l ((l := t) : rest) t n

instance
    {-# OVERLAPPABLE #-}
    (KnownNat n, KnownNat m, Increment n m, RowHasField l rest t n) =>
    RowHasField l (other : rest) t m

-- HasField
-- TODO this gives UGLY error msg, but seems to work just fine.
instance (KnownNat n, RowHasField l types t n) => HasField l (Row types) t where
    getField :: (KnownNat n, RowHasField l types t n) => Row types -> t
    getField (Row xs) =
        unsafeCoerce $ xs !! (fromIntegral @Natural @Int (natVal (Proxy @n)) - 1)
    {-# INLINE getField #-}
