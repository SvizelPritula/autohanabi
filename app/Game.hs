module Game where

import Cards (Card, CardNumber, CardVec, ColorVec, Deck, drawCard, startingDeck)
import Control.Monad (replicateM)
import Control.Monad.State.Strict (State, StateT (runStateT))
import Data.Maybe (fromJust)
import System.Random (RandomGen)
import System.Random.Stateful (StateGenM)
import Vec (Vec (Index, allSame, fromIndex, (!)))

data GameState = GameState
  { deck :: Deck,
    piles :: ColorVec (Maybe CardNumber),
    hands :: PlayerVec [CardState],
    informationTokens :: Int,
    fuseTokens :: Int
  }
  deriving (Show, Eq)

data CardState = CardState {actual :: Card, knowledge :: CardVec Bool} deriving (Show, Eq)

data Player = Human | Computer deriving (Show, Eq)

data PlayerVec a = PlayerVec a a deriving (Show, Eq)

instance Vec PlayerVec where
  type Index PlayerVec = Player

  (!) (PlayerVec v _) Human = v
  (!) (PlayerVec _ v) Computer = v

  fromIndex f = PlayerVec (f Human) (f Computer)

otherPlayer :: Player -> Player
otherPlayer Computer = Human
otherPlayer Human = Computer

cardInHandCount :: Int
cardInHandCount = 4

drawStartingHand :: (RandomGen g) => StateGenM g -> StateT Deck (State g) (PlayerVec [CardState])
drawStartingHand rng = do
  let drawHandCards = replicateM cardInHandCount (fmap fromJust (drawCard rng))
  let drawHand = fmap (map (\actual -> CardState {actual, knowledge = allSame (allSame True)})) drawHandCards
  human <- drawHand
  computer <- drawHand
  return (PlayerVec human computer)

genStartingState :: (RandomGen g) => StateGenM g -> State g GameState
genStartingState rng = do
  (hands, deck) <- runStateT (drawStartingHand rng) startingDeck
  return
    GameState
      { deck,
        hands,
        piles = allSame Nothing,
        informationTokens = 8,
        fuseTokens = 3
      }
