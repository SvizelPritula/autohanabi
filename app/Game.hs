module Game where

import Cards (Card (Card), CardColor, CardNumber, CardVec, ColorVec, Deck, drawCard, startingDeck)
import Control.Monad (replicateM)
import Control.Monad.State.Strict (State, StateT (runStateT))
import Data.Maybe (fromJust, maybeToList)
import System.Random (RandomGen)
import System.Random.Stateful (StateGenM)
import Vec (Vec (Index, allSame, fromIndex, set, vmapWithKey, (!)))

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

data Action = Play Int | Discard Int | Hint Hint deriving (Show, Eq)

data Hint = ColorHint CardColor | NumberHint CardNumber deriving (Show, Eq)

instance Vec PlayerVec where
  type Index PlayerVec = Player

  (!) (PlayerVec v _) Human = v
  (!) (PlayerVec _ v) Computer = v

  fromIndex f = PlayerVec (f Human) (f Computer)

otherPlayer :: Player -> Player
otherPlayer Computer = Human
otherPlayer Human = Computer

cardToCardState :: Card -> CardState
cardToCardState actual = CardState {actual, knowledge = allSame (allSame True)}

handSize :: Int
handSize = 4

maxInformationTokens :: Int
maxInformationTokens = 8

maxFuseTokens :: Int
maxFuseTokens = 3

drawStartingHand :: (RandomGen g) => StateGenM g -> StateT Deck (State g) (PlayerVec [CardState])
drawStartingHand rng = do
  let drawHandCards = replicateM handSize (fmap fromJust (drawCard rng))
  let drawHand = fmap (map cardToCardState) drawHandCards
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
        informationTokens = maxInformationTokens,
        fuseTokens = maxFuseTokens
      }

removeNth :: Int -> [a] -> (a, [a])
removeNth idx list =
  case splitAt idx list of
    (start, (el : end)) -> (el, start ++ end)
    (_, []) -> error "List is too short"

pileTargetNumber :: Maybe CardNumber -> Maybe CardNumber
pileTargetNumber (Nothing) = Just (minBound)
pileTargetNumber (Just number)
  | number < maxBound = Just (succ number)
  | otherwise = Nothing

takeCardFromHand :: (RandomGen g) => GameState -> Player -> Int -> StateGenM g -> State g (Card, GameState)
takeCardFromHand state player idx rng = do
  let (cardState, remainingHand) = removeNth idx (hands state ! player)
  (newCard, newDeck) <- runStateT (drawCard rng) (deck state)
  let newHand = remainingHand ++ maybeToList (fmap cardToCardState newCard)
  return
    ( actual cardState,
      state {deck = newDeck, hands = set player newHand (hands state)}
    )

matchesHint :: Hint -> Card -> Bool
matchesHint (ColorHint color) (Card c _) = c == color
matchesHint (NumberHint number) (Card _ n) = n == number

play :: (RandomGen g) => GameState -> Player -> Action -> StateGenM g -> State g GameState
play state player (Play idx) rng = do
  (Card color number, newState) <- takeCardFromHand state player idx rng
  if Just number == pileTargetNumber (piles state ! color)
    then return newState {piles = set color (Just number) (piles state)}
    else return newState {fuseTokens = fuseTokens state - 1}
play state player (Discard idx) rng = do
  (_, newState) <- takeCardFromHand state player idx rng
  let newTokens = min (informationTokens state + 1) maxInformationTokens
  return newState {informationTokens = newTokens}
play state player (Hint hint) _rng =
  let matches = matchesHint hint
      filterKnowledge shouldMatch = vmapWithKey (\color -> vmapWithKey (\number p -> p && (matches (Card color number) == shouldMatch)))
      mapCardState cardState = cardState {knowledge = filterKnowledge (matches $ actual cardState) (knowledge cardState)}
      newHand = map mapCardState (hands state ! (otherPlayer player))
   in return
        state
          { hands = set (otherPlayer player) newHand (hands state),
            informationTokens = informationTokens state - 1
          }
