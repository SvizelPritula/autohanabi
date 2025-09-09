module Ai where

import Cards (Card (Card), CardVec, possibleCards)
import Data.Foldable (find)
import Game (Action (Discard, Hint, Play), CardState (actual, knowledge), GameState (hands, informationTokens, piles), Hint (ColorHint, NumberHint), Player (Computer, Human), cardNumberToInt, enumerate, matchesHint, pileToInt)
import Vec ((!))

pickAction :: GameState -> Action
pickAction state = case tryGuaranteedPlay state of
  Just idx -> Play idx
  Nothing ->
    if informationTokens state > 0
      then
        let possibleHints = filter (\hint -> any (matchesHint hint) (map actual (hands state ! Human))) allHints
         in Hint (head possibleHints)
      else Discard 0

allHints :: [Hint]
allHints =
  [NumberHint number | number <- [minBound .. maxBound]]
    ++ [ColorHint color | color <- [minBound .. maxBound]]

tryGuaranteedPlay :: GameState -> Maybe Int
tryGuaranteedPlay state =
  let canBePlayed (Card color number) = cardNumberToInt number == pileToInt (piles state ! color) + 1
      isGuaranteedPlay card = all canBePlayed (possibleCards card)
   in indexOf isGuaranteedPlay (myHand state)

indexOf :: (b -> Bool) -> [b] -> Maybe Int
indexOf predicate list = fmap fst $ find (uncurry (const predicate)) (enumerate list)

myHand :: GameState -> [CardVec Bool]
myHand = (map knowledge) . (! Computer) . hands
