module Main where

import Control.Monad.State.Strict (State)
import Game (genStartingState)
import System.Random (StdGen, initStdGen)
import System.Random.Stateful (StateGenM, runStateGen_)

runStateGenIO :: (StateGenM StdGen -> State StdGen a) -> IO a
runStateGenIO f = do
  rng <- initStdGen
  return (runStateGen_ rng f)

main :: IO ()
main = do
  state <- runStateGenIO genStartingState
  print state
