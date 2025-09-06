module Main where

import Cards (Deck, drawCard, startingDeck)
import System.Random (RandomGen, initStdGen)

drawAll :: (RandomGen g) => Deck -> g -> IO ()
drawAll deck rng = do
  let (card, newDeck, newRng) = drawCard deck rng
  case card of
    Just cardValue -> do
      print cardValue
      drawAll newDeck newRng
    Nothing -> return ()

main :: IO ()
main = do
  rng <- initStdGen
  drawAll startingDeck rng
