module Cards where

import Ansi (makeBlue, makeGreen, makeRed, makeWhite, makeYellow)
import Control.Monad.State.Strict (MonadState (get, put), MonadTrans (lift), State, StateT)
import Data.Maybe (fromJust)
import System.Random (RandomGen)
import System.Random.Stateful (StateGenM, UniformRange (uniformRM))
import Vec (Vec (Index, allSame, fromIndex, set, toList, toListWithKey, (!)))

data CardColor = Red | Yellow | Green | Blue | White deriving (Eq, Ord, Enum, Bounded)

data CardNumber = One | Two | Three | Four | Five deriving (Eq, Ord, Enum, Bounded)

data Card = Card CardColor CardNumber deriving (Eq, Bounded)

numberCount :: Int
numberCount = fromEnum (maxBound :: CardNumber) + 1

instance Enum Card where
  toEnum i = Card (toEnum (i `quot` numberCount)) (toEnum (i `rem` numberCount))
  fromEnum (Card color number) = fromEnum number + numberCount * fromEnum color

data ColorVec a = ColorVec a a a a a deriving (Show, Eq, Ord)

data NumberVec a = NumberVec a a a a a deriving (Show, Eq, Ord)

newtype CardVec a = CardVec (ColorVec (NumberVec a)) deriving (Show, Eq, Ord)

type Deck = CardVec Int

cardColor :: Card -> CardColor
cardColor (Card color _) = color

cardNumber :: Card -> CardNumber
cardNumber (Card _ number) = number

unwrapCardVec :: CardVec a -> ColorVec (NumberVec a)
unwrapCardVec (CardVec inner) = inner

startingDeck :: Deck
startingDeck = CardVec $ allSame (NumberVec 3 2 2 2 1)

instance Vec ColorVec where
  type Index ColorVec = CardColor

  (!) (ColorVec v _ _ _ _) Red = v
  (!) (ColorVec _ v _ _ _) Yellow = v
  (!) (ColorVec _ _ v _ _) Green = v
  (!) (ColorVec _ _ _ v _) Blue = v
  (!) (ColorVec _ _ _ _ v) White = v

  fromIndex f = ColorVec (f Red) (f Yellow) (f Green) (f Blue) (f White)

instance Vec NumberVec where
  type Index NumberVec = CardNumber

  (!) (NumberVec v _ _ _ _) One = v
  (!) (NumberVec _ v _ _ _) Two = v
  (!) (NumberVec _ _ v _ _) Three = v
  (!) (NumberVec _ _ _ v _) Four = v
  (!) (NumberVec _ _ _ _ v) Five = v

  fromIndex f = NumberVec (f One) (f Two) (f Three) (f Four) (f Five)

instance Vec CardVec where
  type Index CardVec = Card

  (!) (CardVec vecs) (Card color number) = vecs ! color ! number

  fromIndex f = CardVec $ fromIndex (\color -> fromIndex (f . Card color))

colored :: CardColor -> String -> String
colored Red = makeRed
colored Yellow = makeYellow
colored Green = makeGreen
colored Blue = makeBlue
colored White = makeWhite

colorName :: CardColor -> String
colorName Red = "red"
colorName Yellow = "yellow"
colorName Green = "green"
colorName Blue = "blue"
colorName White = "white"

instance Show CardColor where
  show color = colored color $ colorName color

instance Show CardNumber where
  show One = "1"
  show Two = "2"
  show Three = "3"
  show Four = "4"
  show Five = "5"

instance Show Card where
  show (Card color number) = colored color (show number)

longCardName :: Card -> String
longCardName (Card color number) = colored color (colorName color ++ " " ++ show number)

possibleCards :: CardVec Bool -> [Card]
possibleCards knowledge = [card | card <- [minBound .. maxBound], knowledge ! card]

indexWithWeight :: [(a, Int)] -> Int -> Maybe a
indexWithWeight [] _ = Nothing
indexWithWeight ((el, count) : rest) idx
  | idx < count = Just el
  | otherwise = indexWithWeight rest (idx - count)

deckSize :: Deck -> Int
deckSize deck = sum $ toList deck

drawCard :: (RandomGen g) => StateGenM g -> StateT Deck (State g) (Maybe Card)
drawCard rng = do
  deck <- get
  let cards = toListWithKey deck
  let cardCount = sum (map snd cards)
  if cardCount == 0
    then
      return Nothing
    else do
      idx <- lift (uniformRM (0, cardCount - 1) rng)
      let card = fromJust (indexWithWeight cards idx)
      let newCount = deck ! card - 1
      put (set card newCount deck)
      return (Just card)
