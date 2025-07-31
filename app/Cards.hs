{-# LANGUAGE TypeFamilies #-}

module Cards where

import Data.Kind (Type)

data CardColor = Red | Yellow | Green | Blue | White deriving (Show, Eq)

data CardNumber = One | Two | Three | Four | Five deriving (Show, Eq)

data Card = Card CardColor CardNumber deriving (Show, Eq)

data ColorVec a = ColorVec a a a a a deriving (Show, Eq)

data NumberVec a = NumberVec a a a a a deriving (Show, Eq)

class Vec a where
  type Index a :: Type

  fromIndex :: (Index a -> v) -> a v
  (!) :: a v -> Index a -> v

  allSame :: v -> a v
  allSame = fromIndex . const

  set :: (Eq (Index a)) => Index a -> v -> a v -> a v
  set index value original = fromIndex (\i -> if i == index then value else original ! i)

  vmap :: (t -> t) -> a t -> a t
  vmap f original = fromIndex (\i -> f (original ! i))

instance Vec ColorVec where
  type Index ColorVec = CardColor

  (!) (ColorVec v _ _ _ _) Red = v
  (!) (ColorVec _ v _ _ _) Yellow = v
  (!) (ColorVec _ _ v _ _) Green = v
  (!) (ColorVec _ _ _ v _) Blue = v
  (!) (ColorVec _ _ _ _ v) White = v

  fromIndex f = ColorVec (f Red) (f Blue) (f Green) (f Yellow) (f White)

instance Vec NumberVec where
  type Index NumberVec = CardNumber

  (!) (NumberVec v _ _ _ _) One = v
  (!) (NumberVec _ v _ _ _) Two = v
  (!) (NumberVec _ _ v _ _) Three = v
  (!) (NumberVec _ _ _ v _) Four = v
  (!) (NumberVec _ _ _ _ v) Five = v

  fromIndex f = NumberVec (f One) (f Two) (f Three) (f Four) (f Five)
