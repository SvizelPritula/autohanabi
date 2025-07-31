module Main where

import Cards (CardColor (Blue), ColorVec, Vec (allSame, set))

vec :: (Vec a) => a Integer
vec = allSame 1

main :: IO ()
main = print (set Blue 2 (vec :: ColorVec Integer))
