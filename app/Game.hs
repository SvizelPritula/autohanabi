module Game where

import Cards (Card (Card), CardColor, CardNumber, CardVec, ColorVec, Deck, NumberVec, cardColor, cardNumber, deckSize, drawCard, startingDeck)
import Control.Monad (replicateM)
import Control.Monad.State.Strict (MonadState (get, put), MonadTrans (lift), State, StateT (runStateT))
import Data.Maybe (fromJust, maybeToList)
import System.Random (RandomGen)
import System.Random.Stateful (StateGenM)
import Utils (enumerate, removeNth)
import Vec (Vec (Index, allSame, fromIndex, set, toList, vmapWithKey, (!)))

data GameState = GameState
  { deck :: Deck,
    piles :: ColorVec (Maybe CardNumber),
    hands :: PlayerVec [CardState],
    infoTokens :: Int,
    fuseTokens :: Int,
    gameEndCountdown :: Maybe Int
  }
  deriving (Show, Eq)

data CardState = CardState {actual :: Card, knowledge :: Knowledge} deriving (Show, Eq)

data Knowledge = Knowledge (ColorVec Bool) (NumberVec Bool) deriving (Show, Eq)

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

noKnowledge :: Knowledge
noKnowledge = Knowledge (allSame True) (allSame True)

cardToCardState :: Card -> CardState
cardToCardState actual = CardState {actual, knowledge = noKnowledge}

handSize :: Int
handSize = 4

maxInfoTokens :: Int
maxInfoTokens = 8

maxFuseTokens :: Int
maxFuseTokens = 3

allHints :: [Hint]
allHints = map NumberHint [minBound .. maxBound] ++ map ColorHint [minBound .. maxBound]

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
        infoTokens = maxInfoTokens,
        fuseTokens = maxFuseTokens,
        gameEndCountdown = Nothing
      }

takeCardFromHand :: (RandomGen g) => Player -> Int -> StateGenM g -> StateT GameState (State g) Card
takeCardFromHand player idx rng = do
  state <- get

  let (cardState, remainingHand) = removeNth idx (hands state ! player)
  (newCard, newDeck) <- lift $ runStateT (drawCard rng) (deck state)
  let newHand = remainingHand ++ maybeToList (fmap cardToCardState newCard)

  let newCountdown =
        if deckSize newDeck == 0 && gameEndCountdown state == Nothing
          then Just 2
          else gameEndCountdown state

  put state {deck = newDeck, hands = set player newHand (hands state), gameEndCountdown = newCountdown}
  return $ actual cardState

decrementGameEndCountdown :: (Monad m) => StateT GameState m ()
decrementGameEndCountdown = do
  state <- get
  let newCountdown = fmap (subtract 1) $ gameEndCountdown state
  put state {gameEndCountdown = newCountdown}

updateKnowledgePart :: (Vec a, Eq (Index a)) => Index a -> Bool -> a Bool -> a Bool
updateKnowledgePart target matches = vmapWithKey (\idx p -> p && ((idx == target) == matches))

filterKnowledge :: Hint -> Card -> Knowledge -> Knowledge
filterKnowledge (ColorHint color) card (Knowledge colorK numberK) =
  Knowledge (updateKnowledgePart color (cardColor card == color) colorK) numberK
filterKnowledge (NumberHint number) card (Knowledge colorK numberK) =
  Knowledge colorK (updateKnowledgePart number (cardNumber card == number) numberK)

knowledgeToPossible :: Knowledge -> CardVec Bool
knowledgeToPossible (Knowledge colorK numberK) = fromIndex (\(Card c n) -> colorK ! c && numberK ! n)

play :: (RandomGen g) => Player -> Action -> StateGenM g -> StateT GameState (State g) ActionResult
play player (Play idx) rng = do
  decrementGameEndCountdown
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
  decrementGameEndCountdown
  card <- takeCardFromHand player idx rng
  state <- get
  put $ state {infoTokens = min (infoTokens state + 1) maxInfoTokens}
  return $ Discarded card
play player (Hint hint) _rng = do
  decrementGameEndCountdown
  state <- get
  let coplayerCards = hands state ! (otherPlayer player)
  let updateCardState cardState = cardState {knowledge = filterKnowledge hint (actual cardState) (knowledge cardState)}
      newHand = map updateCardState coplayerCards
  put
    state
      { hands = set (otherPlayer player) newHand (hands state),
        infoTokens = infoTokens state - 1
      }
  return $
    Hinted hint (map fst $ filter (uncurry $ const $ matchesHint hint) (enumerate $ map actual coplayerCards))

hasGameEnded :: GameState -> Bool
hasGameEnded state =
  maybe False (<= 0) (gameEndCountdown state)
    || fuseTokens state <= 0
    || all (== (Just maxBound)) (toList $ piles state)
