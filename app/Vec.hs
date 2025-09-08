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

  vmapWithKey :: (Index a -> t -> u) -> a t -> a u
  vmapWithKey f original = fromIndex (\i -> f i (original ! i))

  vmap :: (t -> u) -> a t -> a u
  vmap = vmapWithKey . const

  toListWithKey :: (Bounded (Index a), Enum (Index a)) => a b -> [(Index a, b)]
  toListWithKey vec = map (\i -> (i, vec ! i)) [minBound .. maxBound]

  toList :: (Bounded (Index a), Enum (Index a)) => a b -> [b]
  toList = (map (snd)) . toListWithKey

vfoldl :: (Vec v, Bounded (Index v), Enum (Index v)) => (b -> a -> b) -> b -> v a -> b
vfoldl f zero vec = foldl f zero (toList vec)
