{-# LANGUAGE TypeFamilies #-}

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

  vmap :: (t -> t) -> a t -> a t
  vmap f original = fromIndex (\i -> f (original ! i))
