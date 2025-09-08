module Main where

import Ansi (makeBlue, makeGray, makeRed)
import Control.Monad (forM_)
import Control.Monad.State.Strict (State)
import Game (CardState (actual), GameState (fuseTokens, hands, informationTokens), Player (Computer, Human), genStartingState, maxFuseTokens, maxInformationTokens)
import System.Random (StdGen, initStdGen)
import System.Random.Stateful (StateGenM, runStateGen_)
import Vec ((!))

runStateGenIO :: (StateGenM StdGen -> State StdGen a) -> IO a
runStateGenIO f = do
  rng <- initStdGen
  return (runStateGen_ rng f)

printGameState :: GameState -> IO ()
printGameState state = do
  let putElement = putStr . (' ' :)

  let printLine title contents = do
        putStr title
        forM_ contents putElement
        putChar '\n'

  let token color True = color "*"
      token _ False = makeGray "*"

  printLine "Computer:" (map (show . actual) (hands state ! Computer))
  printLine "You:     " (map (const (makeGray "?")) (hands state ! Human))

  printLine "Info:    " [token makeBlue (i <= informationTokens state) | i <- [1 .. maxInformationTokens]]
  printLine "Fuse:    " [token makeRed (i <= fuseTokens state) | i <- [1 .. maxFuseTokens]]

main :: IO ()
main = do
  state <- runStateGenIO genStartingState
  printGameState state
