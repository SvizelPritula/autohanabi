module Game where

import Cards (Card (Card), CardColor, CardNumber, CardVec, ColorVec, Deck, drawCard, startingDeck)
import Control.Monad (replicateM)
import Control.Monad.State.Strict (MonadState (get, put), MonadTrans (lift), State, StateT (runStateT))
import Data.Maybe (fromJust, maybeToList)
import System.Random (RandomGen)
import System.Random.Stateful (StateGenM)
import Vec (Vec (Index, allSame, fromIndex, set, vmapWithKey, (!)))

data GameState = GameState
  { deck :: Deck,
    piles :: ColorVec (Maybe CardNumber),
    hands :: PlayerVec [CardState],
    infoTokens :: Int,
    fuseTokens :: Int
  }
  deriving (Show, Eq)

data CardState = CardState {actual :: Card, knowledge :: CardVec Bool} deriving (Show, Eq)

data Player = Human | Computer deriving (Show, Eq)

data PlayerVec a = PlayerVec a a deriving (Show, Eq)

data Action = Play Int | Discard Int | Hint Hint deriving (Show, Eq)

data Hint = ColorHint CardColor | NumberHint CardNumber deriving (Show, Eq)

data ActionResult = Played Card Bool | Discarded Card | Hinted Hint [Int]

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

maxinfoTokens :: Int
maxinfoTokens = 8

maxFuseTokens :: Int
maxFuseTokens = 3

removeNth :: Int -> [a] -> (a, [a])
removeNth idx list =
  case splitAt idx list of
    (start, (el : end)) -> (el, start ++ end)
    (_, []) -> error "List is too short"

enumerate :: [a] -> [(Int, a)]
enumerate = zip [0 ..]

matchesHint :: Hint -> Card -> Bool
matchesHint (ColorHint color) (Card c _) = c == color
matchesHint (NumberHint number) (Card _ n) = n == number

cardNumberToInt :: CardNumber -> Int
cardNumberToInt number = fromEnum number + 1

pileToInt :: Maybe CardNumber -> Int
pileToInt (Nothing) = 0
pileToInt (Just number) = cardNumberToInt number

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
        infoTokens = maxinfoTokens,
        fuseTokens = maxFuseTokens
      }

takeCardFromHand :: (RandomGen g) => Player -> Int -> StateGenM g -> StateT GameState (State g) Card
takeCardFromHand player idx rng = do
  state <- get
  let (cardState, remainingHand) = removeNth idx (hands state ! player)
  (newCard, newDeck) <- lift $ runStateT (drawCard rng) (deck state)
  let newHand = remainingHand ++ maybeToList (fmap cardToCardState newCard)
  put state {deck = newDeck, hands = set player newHand (hands state)}
  return $ actual cardState

play :: (RandomGen g) => Player -> Action -> StateGenM g -> StateT GameState (State g) ActionResult
play player (Play idx) rng = do
  Card color number <- takeCardFromHand player idx rng
  state <- get
  if cardNumberToInt number == pileToInt (piles state ! color) + 1
    then do
      put
        state
          { piles = set color (Just number) (piles state),
            infoTokens = infoTokens state + if number == maxBound then 1 else 0
          }
      return $ Played (Card color number) True
    else do
      put state {fuseTokens = fuseTokens state - 1}
      return $ Played (Card color number) False
play player (Discard idx) rng = do
  card <- takeCardFromHand player idx rng
  state <- get
  put $ state {infoTokens = min (infoTokens state + 1) maxinfoTokens}
  return $ Discarded card
play player (Hint hint) _rng =
  do
    state <- get
    let coplayerCards = hands state ! (otherPlayer player)
    let matches = matchesHint hint
        filterKnowledge shouldMatch = vmapWithKey (\color -> vmapWithKey (\number p -> p && (matches (Card color number) == shouldMatch)))
        updateCardState cardState = cardState {knowledge = filterKnowledge (matches $ actual cardState) (knowledge cardState)}
        newHand = map updateCardState coplayerCards
    put
      state
        { hands = set (otherPlayer player) newHand (hands state),
          infoTokens = infoTokens state - 1
        }
    return $
      Hinted hint (map fst $ filter (uncurry $ const matches) (enumerate $ map actual coplayerCards))
