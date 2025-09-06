module Game where

import Cards (Card, CardNumber, CardVec, ColorVec, Deck)
import Vec (Vec (Index, fromIndex, (!)))

data State = State
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
