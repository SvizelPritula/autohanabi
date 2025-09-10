module Ai where

import Cards (Card (Card), CardVec, possibleCards)
import Data.Foldable (find)
import Data.List (sortOn)
import Game (Action (Discard, Hint, Play), CardState (actual, knowledge), GameState (hands, infoTokens, piles), Hint (ColorHint, NumberHint), Player (Computer, Human), cardNumberToInt, enumerate, filterKnowledge, matchesHint, pileToInt)
import Vec (Vec (toList), (!))

pickAction :: GameState -> Action
pickAction state = case tryGuaranteedPlay state of
  Just idx -> Play idx
  Nothing -> case tryGuaranteedDiscard state of
    Just idx -> Discard idx
    Nothing ->
      if infoTokens state > 0
        then
          let humanCards = map actual (hands state ! Human)
              possibleHints = filter (\hint -> any (matchesHint hint) humanCards) allHints
              scoreForCard hint card = length $ filter not $ toList $ filterKnowledge hint (actual card) (knowledge card)
              score hint = sum $ map (scoreForCard hint) (hands state ! Human)
              hints = sortOn score possibleHints
           in Hint $ last hints
        else Discard 0

allHints :: [Hint]
allHints =
  [NumberHint number | number <- [minBound .. maxBound]]
    ++ [ColorHint color | color <- [minBound .. maxBound]]

tryGuaranteed :: (Int -> Int -> Bool) -> GameState -> Maybe Int
tryGuaranteed condition state =
  let canBePlayed (Card color number) = condition (cardNumberToInt number) (pileToInt (piles state ! color))
      isGuaranteed card = all canBePlayed (possibleCards card)
   in indexOf isGuaranteed (myHand state)

tryGuaranteedPlay :: GameState -> Maybe Int
tryGuaranteedPlay = tryGuaranteed (\c p -> c == p + 1)

tryGuaranteedDiscard :: GameState -> Maybe Int
tryGuaranteedDiscard = tryGuaranteed (\c p -> c <= p)

indexOf :: (b -> Bool) -> [b] -> Maybe Int
indexOf predicate list = fmap fst $ find (uncurry (const predicate)) (enumerate list)

myHand :: GameState -> [CardVec Bool]
myHand = (map knowledge) . (! Computer) . hands
