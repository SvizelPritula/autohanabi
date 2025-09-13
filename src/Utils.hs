module Utils where

import Data.Foldable (find)

removeNth :: Int -> [a] -> (a, [a])
removeNth idx list =
  case splitAt idx list of
    (start, el : end) -> (el, start ++ end)
    (_, []) -> error "List is too short"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

indexOf :: (b -> Bool) -> [b] -> Maybe Int
indexOf predicate list = fst <$> find (uncurry (const predicate)) (enumerate list)

infinity :: Double
infinity = 1 / 0
