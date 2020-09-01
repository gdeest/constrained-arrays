{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE UndecidableInstances #-}

module ConstrainedArrays where

import Data.Kind (Constraint, Type)
import Data.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)
import qualified Fcf
import GHC.TypeLits

import Data.Vector (Vector)
import qualified Data.Vector as Vector

data SNat = Z | S SNat

type D0 = Z
type D1 = S D0
type D2 = S D1
type D3 = S D2
type D4 = S D3
type D5 = S D4

data Ix' t (n :: SNat) where
  Iz   :: Ix' t Z
  (:-) :: t -> Ix' t n -> Ix' t (S n)

infixr :-

type Ix = Ix' Nat
type IxTerm = Ix' Integer

data Arr (n :: SNat) (constraints :: Ix n -> Fcf.Exp Bool) a =
  Arr
    { values  :: (Vector a)
    , mapping :: IxTerm n -> Int
    }


data (.:.)
  :: (b -> Fcf.Exp c)
  -> (a -> Fcf.Exp b)
  -> (b -> Fcf.Exp c)

data (&:&)
  :: (Ix n -> Fcf.Exp Bool)
  -> (Ix n -> Fcf.Exp Bool)
  -> (Ix n -> Fcf.Exp Bool)

data (|:|)
  :: (Ix n -> Fcf.Exp Bool)
  -> (Ix n -> Fcf.Exp Bool)
  -> (Ix n -> Fcf.Exp Bool)

data PNot
  :: (a -> Fcf.Exp Bool)
  -> (a -> Fcf.Exp Bool)

data LowerTrig
  :: Ix D2 -> Fcf.Exp Bool

data RectiLinear :: Ix n -> Ix n -> Fcf.Exp Bool
data Transpose :: Ix D2 -> Fcf.Exp (Ix D2)

type InBounds l u i = And (l <=? i) (i+1 <=? u)

type instance Fcf.Eval (RectiLinear Iz Iz) = True
type instance Fcf.Eval (RectiLinear (b ':- bs) (i ':- is)) =
  And (InBounds 0 b i) (Fcf.Eval (RectiLinear bs is))

type family And (a :: Bool) (b :: Bool) where
  And True True = True
  And _ _ = False

type family Or (a :: Bool) (b :: Bool) where
  Or True _ = True
  Or _ b = b

type family Not (a :: Bool) where
  Not False = True
  Not True = False

type instance Fcf.Eval ((c1 &:& c2) ix) = And (Fcf.Eval (c1 ix)) (Fcf.Eval (c2 ix))
type instance Fcf.Eval ((c1 |:| c2) ix) = Or (Fcf.Eval (c1 ix)) (Fcf.Eval (c2 ix))
type instance Fcf.Eval ((PNot p) ix) = Not (Fcf.Eval (p ix))

type instance Fcf.Eval ((g .:. f) a) = Fcf.Eval (g (Fcf.Eval (f a)))

type instance Fcf.Eval (Transpose (i ':- j ':- Iz)) =
  (j ':- i ':- Iz)

type instance Fcf.Eval (LowerTrig (i ':- j ':- Iz)) =
  j <=? i

class KnownIx (ix :: Ix n) where
  ix :: (IxTerm n)

instance KnownIx Iz where
  ix = Iz

instance (KnownNat i, KnownIx is) => KnownIx (i ':- is) where
  ix = (natVal (Proxy @i)) :- (ix @_ @is)

at
  :: forall n ix constraints proxy a.
     ( Fcf.Eval (constraints ix) ~ True, KnownIx ix )
  => Arr n constraints a
  -> proxy ix
  -> a
at Arr {values, mapping} _ = values Vector.! mapping (ix @_ @ix)

restrict
  :: forall n constraints' constraints a.
     Arr n constraints a
  -> Arr n (constraints &:& constraints') a
restrict = unsafeCoerce

transpose
  :: forall n constraints a.
     Arr D2 constraints a
  -> Arr D2 (constraints .:. Transpose) a
transpose Arr {mapping, values} = Arr
  { values
  , mapping = mapping . (\(i :- j :- Iz) -> (j :- i :- Iz))
  }

split
  :: forall n (predicate :: Ix n -> Fcf.Exp Bool) constraints a.
     Arr n constraints a
  -> ( Arr n (constraints &:& predicate) a
     , Arr n (constraints &:& (PNot predicate)) a
     )
split arr = ( restrict @_ @predicate arr, restrict @_ @(PNot predicate) arr )
