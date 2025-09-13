module Vec where

import Data.Kind (Type)

class Vec a where
  type Index a :: Type

  fromIndex :: (Index a -> v) -> a v
  (!) :: a v -> Index a -> v

  allSame :: v -> a v
  allSame = fromIndex . const

  change :: (Eq (Index a)) => Index a -> (v -> v) -> a v -> a v
  change index f original = fromIndex (\i -> if i == index then f $ original ! i else original ! i)

  set :: (Eq (Index a)) => Index a -> v -> a v -> a v
  set index value = change index (const value)

  vmapWithKey :: (Index a -> t -> u) -> a t -> a u
  vmapWithKey f original = fromIndex (\i -> f i (original ! i))

  vmap :: (t -> u) -> a t -> a u
  vmap = vmapWithKey . const

  vzipWith :: (t -> u -> v) -> a t -> a u -> a v
  vzipWith f a b = fromIndex (\i -> f (a ! i) (b ! i))

  toListWithKey :: (Bounded (Index a), Enum (Index a)) => a b -> [(Index a, b)]
  toListWithKey vec = map (\i -> (i, vec ! i)) [minBound .. maxBound]

  toList :: (Bounded (Index a), Enum (Index a)) => a b -> [b]
  toList = map snd . toListWithKey

  vcount :: (Bounded (Index a), Enum (Index a)) => (b -> Bool) -> a b -> Int
  vcount f vec = length $ filter f $ toList vec
