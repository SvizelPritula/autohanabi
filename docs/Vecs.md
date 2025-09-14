# The `Vec` class

Autohanabi defines a custom class called `Vec`,
instances of which are heavily used throughout the entire codebase.
`Vec` makes use of
[Type Families](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/type_families.html).
An instance `a` represents a container that is indexed by an associated type,
`Index a`, which is usually an enum.
If `Vec a`, then `a v` can be thought of as a dictionary/map whose keys are of type `Index a`
and whose values are of type `v`.
It always has a value for every possible key.

For example, `ColorVec a` stores five values, one for each possible `CardColor`.
Its definition couldn't be simpler:

```hs
data CardColor = Red | Yellow | Green | Blue | White -- The key
data ColorVec a = ColorVec a a a a a -- The dictionary
```

The arguments of its data constructor correspond to the values for
`Red`, `Yellow`, `Green`, `Blue` and `White`, in that order.

`Vec` has only two required methods.
The first is the indexing operator `(!) :: Index a -> a -> v`.
The second is the constructor `fromIndex :: (Index a -> v) -> a v`,
whose argument is a function used to compute the initial value for each key.

For example, a complete definition of `instance Vec ColorVec` looks like this:

```hs
instance Vec ColorVec where
  type Index ColorVec = CardColor

  (!) (ColorVec v _ _ _ _) Red = v
  (!) (ColorVec _ v _ _ _) Yellow = v
  (!) (ColorVec _ _ v _ _) Green = v
  (!) (ColorVec _ _ _ v _) Blue = v
  (!) (ColorVec _ _ _ _ v) White = v

  fromIndex f = ColorVec (f Red) (f Yellow) (f Green) (f Blue) (f White)
```

`Vec` also has many methods with default implementations,
notably `change :: Index a -> (v -> v) -> a v -> a v`
and `set :: Index a -> v -> a v -> a v`,
which allow you to construct a new `Vec` with only one item changed.
There is also `toList` and `toListWithKey`, which convert a `Vec` to a normal list
for use with usual stream processing functions.
Some methods with default implementations are called very often by the AI,
those are sometimes implemented explicitly to improve performance.

Aside from `ColorVec`, there is also `NumberVec` and `CardVec`.
`CardVec a` is just a newtype wrapper around `ColorVec (NumberVec a)`.
Lastly, there is `PlayerVec`.
