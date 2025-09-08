module Cards where

import Ansi (makeBlue, makeGreen, makeRed, makeWhite, makeYellow)
import Control.Monad.State.Strict (MonadState (get, put), MonadTrans (lift), State, StateT)
import Data.Maybe (fromJust)
import System.Random (RandomGen)
import System.Random.Stateful (StateGenM, UniformRange (uniformRM))
import Vec (Vec (Index, allSame, fromIndex, set, (!)))

data CardColor = Red | Yellow | Green | Blue | White deriving (Eq, Ord, Enum, Bounded)

data CardNumber = One | Two | Three | Four | Five deriving (Eq, Ord, Enum, Bounded)

data Card = Card CardColor CardNumber deriving (Eq)

cardColor :: Card -> CardColor
cardColor (Card color _) = color

cardNumber :: Card -> CardNumber
cardNumber (Card _ number) = number

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

colored :: CardColor -> String -> String
colored Red = makeRed
colored Yellow = makeYellow
colored Green = makeGreen
colored Blue = makeBlue
colored White = makeWhite

instance Show CardColor where
  show Red = makeRed "Red"
  show Yellow = makeYellow "Yellow"
  show Green = makeGreen "Green"
  show Blue = makeBlue "Blue"
  show White = makeWhite "White"

instance Show CardNumber where
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"

instance Show Card where
  show (Card color number) = colored color (show number)

setCardVec :: Card -> a -> CardVec a -> CardVec a
setCardVec (Card color number) value deck = set color (set number value (deck ! color)) deck

getCardVec :: Card -> CardVec a -> a
getCardVec (Card color number) deck = deck ! color ! number

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

drawCard :: (RandomGen g) => StateGenM g -> StateT Deck (State g) (Maybe Card)
drawCard rng = do
  deck <- get
  let cards = cardsWithCounts deck
  let cardCount = sum (map snd cards)
  if cardCount == 0
    then
      return Nothing
    else do
      idx <- lift (uniformRM (0, cardCount - 1) rng)
      let card = fromJust (indexWithWeight cards idx)
      let newCount = (getCardVec card deck) - 1
      put (setCardVec card newCount deck)
      return (Just card)
