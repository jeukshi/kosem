{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Kosem.PostgreSQL.Internal.Row where

import Data.Data (Proxy (Proxy))
import Data.Kind (Constraint, Type)
import Database.PostgreSQL.LibPQ (Result)
import GHC.Exts (Any)
import GHC.Records
import GHC.TypeError (ErrorMessage (..), TypeError)
import GHC.TypeLits (
    KnownNat,
    KnownSymbol,
    Nat,
    Symbol,
    symbolVal,
    type (+),
 )
import GHC.TypeNats (KnownNat, Nat, Natural, natVal, type (+), type (-))
import Language.Haskell.TH.Syntax (Lift (..))
import Unsafe.Coerce (unsafeCoerce)

type data Pair a b = a := b

-- TODO instances?
-- TODO small array
newtype Row (types :: [Pair Symbol Type]) = Row [Any]

type family Add1 (n :: Nat) :: Nat where
    Add1 n = n + 1

class Increment (n :: Nat) (m :: Nat) | n -> m

instance (KnownNat n, KnownNat m, m ~ Add1 n) => Increment n m

-- TODO, maybe this could be simpler? Maybe Undecidable from GHC 9.8 would help?
type family ErrIfEq (a :: Symbol) (b :: Symbol) :: Constraint where
    ErrIfEq a a =
        TypeError
            ( 'Text "Duplicate field label: "
                ':<>: 'ShowType a
            )
    ErrIfEq a b = ()

class KeyNotIn (l :: Symbol) (xs :: [Pair Symbol Type])

instance KeyNotIn l '[]

instance
    ( ErrIfEq l l'
    , KeyNotIn l xs
    )
    => KeyNotIn l ((l' := x) : xs)

class RowHasField (l :: Symbol) (types :: [Pair Symbol Type]) (t :: Type) (n :: Nat) | l types -> t

instance (KeyNotIn l rest, KnownNat n, Increment 0 n) => RowHasField l ((l := t) : rest) t n

instance
    {-# OVERLAPPABLE #-}
    (KnownNat n, KnownNat m, Increment n m, RowHasField l rest t n)
    => RowHasField l (other : rest) t m

-- HasField
-- TODO this gives UGLY error msg, but seems to work just fine.
instance (KnownNat n, RowHasField l types t n) => HasField l (Row types) t where
    getField :: (KnownNat n, RowHasField l types t n) => Row types -> t
    getField (Row xs) =
        unsafeCoerce $ xs !! (fromIntegral @Natural @Int (natVal (Proxy @n)) - 1)
    {-# INLINE getField #-}
