module Ai where

import Cards (Card (Card), CardNumber, ColorVec, Deck, NumberVec, unwrapCardVec)
import Control.Parallel.Strategies (evalTuple2, parMap, r0, rseq)
import Data.Bifunctor (Bifunctor (bimap, first))
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (groupBy, sortOn)
import Debug.Trace (traceShowId)
import Game (Action (Discard, Hint, Play), CardState (CardState), GameState (GameState), Hint (ColorHint, NumberHint), Knowledge (Knowledge), Player (Computer, Human), allHints, cardNumberToInt, knowledgeToPossible, matchesHint, maxInfoTokens, noKnowledge, pileToInt, updateKnowledgePart)
import Game qualified (CardState (actual, knowledge), GameState (deck, fuseTokens, hands, infoTokens, piles))
import Utils (infinity, removeNth)
import Vec (Vec (change, fromIndex, set, toList, toListWithKey, vzipWith, (!)))

data AiGameState = AiGameState
  { piles :: ColorVec (Maybe CardNumber),
    remainingCards :: Deck,
    ownCards :: [AiCardState],
    coplayerCards :: [AiCardState],
    infoTokens :: Int,
    fuseTokens :: Int
  }
  deriving (Show, Eq, Ord)

data AiCardState = AiCardState {possible :: Deck, knowledge :: Knowledge} deriving (Show, Eq, Ord)

maxDepth :: Int
maxDepth = 3

misfireScore :: Double
misfireScore = -3

playScore :: Double
playScore = 1

potentialLossScore :: Double
potentialLossScore = -1

infoTokenScore :: Double
infoTokenScore = 0.1

pickAction :: GameState -> Action
pickAction = pickActionRec maxDepth . stateToAiState

pickActionRec :: Int -> AiGameState -> Action
pickActionRec depth state =
  let cardIndices = [0 .. length (ownCards state) - 1]
      cardActions = map Discard cardIndices ++ map Play cardIndices
      hintActions = if infoTokens state > 0 && depth > 0 then map Hint allHints else []
      possibleActions = cardActions ++ hintActions
      scoredActions = parMap (evalTuple2 r0 rseq) (\a -> (a, scoreAction depth state a)) possibleActions
   in fst $ maximumBy (compare `on` snd) scoredActions

scoreAction :: Int -> AiGameState -> Action -> Double
scoreAction depth state (Play idx) =
  let updateForPlay baseState (Card color number) =
        if cardNumberToInt number == pileToInt (piles state ! color) + 1
          then (baseState {piles = set color (Just number) (piles state)}, playScore)
          else (baseState {fuseTokens = fuseTokens state - 1}, misfireScore)
   in scoreCardUse updateForPlay depth state idx
scoreAction depth state (Discard idx) =
  let updateForPlay baseState (Card color number) =
        let calcPotential = potential (piles baseState ! color)
            remainingOfColor = unwrapCardVec (remainingCards baseState) ! color
            potentialLoss = calcPotential remainingOfColor - calcPotential (change number (subtract 1) remainingOfColor)
         in (addInfoToken baseState, potentialLossScore * fromIntegral potentialLoss)
   in scoreCardUse updateForPlay depth state idx
scoreAction depth state (Hint hint) =
  let allOutcomesRec 0 = [[]]
      allOutcomesRec i = let r = allOutcomesRec (i - 1) in map (False :) r ++ map (True :) r
      allOutcomes = tail $ allOutcomesRec $ length (coplayerCards state)
      outcomeToState outcome =
        state
          { coplayerCards = zipWith (liftToKnowledge . filterKnowledge hint) outcome (coplayerCards state),
            infoTokens = infoTokens state - 1
          }
      outcomeToScore outcome = scoreStateRec depth $ flipState $ outcomeToState outcome
      weightForCard matches possible = sum $ map snd $ filter ((== matches) . matchesHint hint . fst) $ toListWithKey possible
      outcomeToWeight outcome = product $ zipWith weightForCard outcome (map possible $ coplayerCards state)
      outcomesWithWeights = map (\outcome -> (outcome, outcomeToWeight outcome)) allOutcomes
      possibleOutcomes = filter ((> 0) . snd) outcomesWithWeights
   in case possibleOutcomes of
        [] -> -infinity
        outcomes -> weightedAverage $ map (bimap outcomeToScore fromIntegral) outcomes

scoreCardUse :: (AiGameState -> Card -> (AiGameState, Double)) -> Int -> AiGameState -> Int -> Double
scoreCardUse f depth state idx =
  let baseState = state {ownCards = otherCards ++ [AiCardState {possible = remainingCards state, knowledge = noKnowledge}]}
      (cardState, otherCards) = removeNth idx $ ownCards state
      cardToState = first flipState . f baseState
      cards = filter ((> 0) . snd) $ toListWithKey $ possible cardState
      options = map (bimap cardToState fromIntegral) cards
      states = map (first fst) options
      bias = adjustBias depth $ weightedAverage $ map (first snd) options
   in (+ bias) $
        weightedAverage $
          map (first (scoreStateRec depth)) $
            deduplicateWeighted states

scoreStateRec :: Int -> AiGameState -> Double
scoreStateRec 0 state = scoreStateHeuristic state
scoreStateRec depth state =
  let knownState = state {ownCards = map (cardStateFromKnowledge (remainingCards state) . knowledge) $ ownCards state}
      action = pickActionRec (depth - 1) knownState
   in scoreAction (depth - 1) state action

scoreStateHeuristic :: AiGameState -> Double
scoreStateHeuristic state =
  let infoToken = (* infoTokenScore) $ fromIntegral $ infoTokens state
   in infoToken

deduplicateWeighted :: (Eq a, Ord a, Num b) => [(a, b)] -> [(a, b)]
deduplicateWeighted list =
  let sorted = sortOn fst list
      grouped = groupBy ((==) `on` fst) sorted
      combine same = (fst $ head same, sum $ map snd same)
   in map combine grouped

weightedAverage :: [(Double, Double)] -> Double
weightedAverage elements =
  let totalWeight = sum $ map snd elements
      weightedSum = sum $ map (uncurry (*)) elements
   in weightedSum / totalWeight

potential :: Maybe CardNumber -> NumberVec Int -> Int
potential pile remaining = length $ takeWhile (> 0) $ drop (pileToInt pile) $ toList remaining

adjustBias :: Int -> Double -> Double
adjustBias depth = (* (1 + 0.1 * fromIntegral depth))

liftToKnowledge :: (Knowledge -> Knowledge) -> AiCardState -> AiCardState
liftToKnowledge f state = state {knowledge = f $ knowledge state}

filterKnowledge :: Hint -> Bool -> Knowledge -> Knowledge
filterKnowledge (ColorHint color) matches (Knowledge colorK numberK) =
  Knowledge (updateKnowledgePart color matches colorK) numberK
filterKnowledge (NumberHint number) matches (Knowledge colorK numberK) =
  Knowledge colorK (updateKnowledgePart number matches numberK)

flipState :: AiGameState -> AiGameState
flipState state = state {ownCards = coplayerCards state, coplayerCards = ownCards state}

addInfoToken :: AiGameState -> AiGameState
addInfoToken state
  | infoTokens state < maxInfoTokens = state {infoTokens = infoTokens state + 1}
  | otherwise = state

addToDeck :: Deck -> [Card] -> Deck
addToDeck = foldl (\deck idx -> change idx (+ 1) deck)

removeFromDeck :: Deck -> [Card] -> Deck
removeFromDeck = foldl (\deck idx -> change idx (subtract 1) deck)

cardStateFromKnowledge :: Deck -> Knowledge -> AiCardState
cardStateFromKnowledge remainingCards knowledge =
  let possible = vzipWith (\r k -> r * fromEnum k) remainingCards $ knowledgeToPossible knowledge
   in AiCardState {possible, knowledge}

stateToAiState :: GameState -> AiGameState
stateToAiState GameState {piles, deck, hands, infoTokens, fuseTokens} =
  let remainingCards = addToDeck deck (map Game.actual $ hands ! Computer)
      ownCardToState = cardStateFromKnowledge remainingCards . Game.knowledge
      coplayerCardToState CardState {actual, knowledge} =
        AiCardState {possible = fromIndex (\c -> if c == actual then remainingCards ! actual else 0), knowledge}
   in AiGameState
        { piles,
          remainingCards,
          ownCards = map ownCardToState (hands ! Computer),
          coplayerCards = map coplayerCardToState (hands ! Human),
          infoTokens,
          fuseTokens
        }

traceTop :: (Show a) => Int -> a -> a
traceTop depth
  | depth == maxDepth = traceShowId
  | otherwise = id
