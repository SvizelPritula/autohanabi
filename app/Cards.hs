module Cards where

import Data.Maybe (fromJust)
import System.Random (RandomGen, uniformR)
import Vec (Vec (Index, allSame, fromIndex, set, (!)))

data CardColor = Red | Yellow | Green | Blue | White deriving (Show, Eq, Ord, Enum, Bounded)

data CardNumber = One | Two | Three | Four | Five deriving (Show, Eq, Ord, Enum, Bounded)

data Card = Card CardColor CardNumber deriving (Show, Eq)

data ColorVec a = ColorVec a a a a a deriving (Show, Eq)

data NumberVec a = NumberVec a a a a a deriving (Show, Eq)

type CardVec a = ColorVec (NumberVec a)

type Deck = CardVec Int

startingDeck :: Deck
startingDeck = allSame (NumberVec 3 2 2 2 1)

instance Vec ColorVec where
  type Index ColorVec = CardColor

  (!) (ColorVec v _ _ _ _) Red = v
  (!) (ColorVec _ v _ _ _) Yellow = v
  (!) (ColorVec _ _ v _ _) Green = v
  (!) (ColorVec _ _ _ v _) Blue = v
  (!) (ColorVec _ _ _ _ v) White = v

  fromIndex f = ColorVec (f Red) (f Blue) (f Green) (f Yellow) (f White)

instance Vec NumberVec where
  type Index NumberVec = CardNumber

  (!) (NumberVec v _ _ _ _) One = v
  (!) (NumberVec _ v _ _ _) Two = v
  (!) (NumberVec _ _ v _ _) Three = v
  (!) (NumberVec _ _ _ v _) Four = v
  (!) (NumberVec _ _ _ _ v) Five = v

  fromIndex f = NumberVec (f One) (f Two) (f Three) (f Four) (f Five)

cardsWithCounts :: Deck -> [(Card, Int)]
cardsWithCounts deck =
  [ (Card color number, deck ! color ! number)
  | color <- [minBound .. maxBound],
    number <- [minBound .. maxBound]
  ]

indexWithWeight :: [(a, Int)] -> Int -> Maybe a
indexWithWeight [] _ = Nothing
indexWithWeight ((el, count) : rest) idx
  | idx < count = Just el
  | otherwise = indexWithWeight rest (idx - count)

drawCard :: (RandomGen g) => Deck -> g -> (Maybe Card, Deck, g)
drawCard deck rng =
  let cards = cardsWithCounts deck
      cardCount = sum (map snd cards)
   in if cardCount == 0
        then
          (Nothing, deck, rng)
        else
          let (idx, newRng) = uniformR (0, cardCount - 1) rng
              card = fromJust (indexWithWeight cards idx)
              Card color number = card
              newDeck = set color (set number (deck ! color ! number - 1) (deck ! color)) deck
           in (Just card, newDeck, newRng)
