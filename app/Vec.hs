module Vec where

import Data.Kind (Type)

class Vec a where
  type Index a :: Type

  fromIndex :: (Index a -> v) -> a v
  (!) :: a v -> Index a -> v

  allSame :: v -> a v
  allSame = fromIndex . const

  set :: (Eq (Index a)) => Index a -> v -> a v -> a v
  set index value original = fromIndex (\i -> if i == index then value else original ! i)

  vmap :: (t -> u) -> a t -> a u
  vmap f original = fromIndex (\i -> f (original ! i))

  toListWithKey :: (Bounded (Index a), Enum (Index a)) => a b -> [(Index a, b)]
  toListWithKey vec = map (\i -> (i, vec ! i)) [minBound .. maxBound]

  toList :: (Bounded (Index a), Enum (Index a)) => a b -> [b]
  toList vec = map (vec !) [minBound .. maxBound]

vfoldl :: (Vec v, Bounded (Index v), Enum (Index v)) => (b -> a -> b) -> b -> v a -> b
vfoldl f zero vec = foldl f zero (toList vec)
