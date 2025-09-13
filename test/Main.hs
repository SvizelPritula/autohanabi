module Main where

import Ai (pickAction)
import Cards (Card (Card), CardColor (Blue, Green, Red, White, Yellow), CardNumber (Five, Four, One, Three, Two), CardVec (CardVec), ColorVec (ColorVec), NumberVec (NumberVec), cardColor, cardNumber, deckSize, drawCard, indexWithWeight, startingDeck)
import Control.Exception (evaluate)
import Control.Monad (forM_)
import Control.Monad.State.Strict (StateT (runStateT))
import Data.Maybe (fromJust, isJust)
import Game (Action (Discard, Hint, Play), ActionResult (Hinted), CardState (CardState, actual, knowledge), GameState (GameState, deck, fuseTokens, gameEndCountdown, hands, infoTokens, piles), Hint (ColorHint, NumberHint), Knowledge (Knowledge), Player (Computer, Human), PlayerVec (PlayerVec), genStartingState, handSize, hasGameEnded, maxInfoTokens, noKnowledge, play)
import System.Random (mkStdGen)
import System.Random.Stateful (runStateGen_)
import Test.Hspec (SpecWith, anyException, context, describe, hspec, it, shouldBe, shouldSatisfy, shouldThrow)
import Utils (enumerate, indexOf, infinity, removeNth)
import Vec (Vec (Index, allSame, change, fromIndex, set, toList, vmapWithKey, (!)))

testUtils :: SpecWith ()
testUtils = describe "Utils" $ do
  describe "enumerate" $ do
    it "adds numbers to \"abc\"" $ do
      enumerate "abc" `shouldBe` [(0, 'a'), (1, 'b'), (2, 'c')]

  describe "indexOf" $ do
    it "finds existing" $ do
      indexOf (== 'c') "abcd" `shouldBe` Just 2
    it "finds first among multiple" $ do
      indexOf (>= 'b') "abcd" `shouldBe` Just 1
    it "returns Nothing on no match" $ do
      indexOf (>= 'x') "abcd" `shouldBe` Nothing

  describe "removeNth" $ do
    it "can remove an element" $ do
      removeNth 0 "abcde" `shouldBe` ('a', "bcde")
      removeNth 2 "abcde" `shouldBe` ('c', "abde")
      removeNth 4 "abcde" `shouldBe` ('e', "abcd")
    it "throws an exception when index is out of bounds" $ do
      evaluate (removeNth 3 "abc") `shouldThrow` anyException
      evaluate (removeNth 4 "abc") `shouldThrow` anyException

  describe "infinity" $ do
    it "is very large" $ do
      infinity `shouldSatisfy` (>= 1e300)

data TestEnum = A | B | C deriving (Show, Eq, Ord, Enum, Bounded)

data TestVec a = TestVec a a a deriving (Show, Eq)

instance Vec TestVec where
  type Index TestVec = TestEnum
  (!) (TestVec a _ _) A = a
  (!) (TestVec _ b _) B = b
  (!) (TestVec _ _ c) C = c
  fromIndex f = TestVec (f A) (f B) (f C)

testVec :: SpecWith ()
testVec = describe "Vec" $ do
  describe "allSame" $ do
    it "gives all elements same value" $ do
      allSame "hello" `shouldBe` TestVec "hello" "hello" "hello"

  describe "change" $ do
    it "changes correct element" $ do
      change B ('x' :) (TestVec "a" "b" "c") `shouldBe` TestVec "a" "xb" "c"

  describe "set" $ do
    it "sets correct element" $ do
      set B "x" (TestVec "a" "b" "c") `shouldBe` TestVec "a" "x" "c"

  describe "vmapWithKey" $ do
    it "maps everything correctly" $ do
      vmapWithKey (,) (TestVec 'a' 'b' 'c') `shouldBe` TestVec (A, 'a') (B, 'b') (C, 'c')

  describe "toList" $ do
    it "returns all elements" $ do
      toList (TestVec 'a' 'b' 'c') `shouldBe` "abc"

enumAll :: (Bounded a, Enum a) => [a]
enumAll = [minBound .. maxBound]

testCards :: SpecWith ()
testCards = describe "Cards" $ do
  describe "startingDeck" $ do
    it "has 50 cards" $ do
      deckSize startingDeck `shouldBe` 50

  describe "Enum Card" $ do
    it "enumerates all cards" $ do
      enumAll `shouldBe` [Card color number | color <- enumAll, number <- enumAll]

  describe "indexWithWeight" $ do
    it "findsAllCards" $ do
      let cards = [('a', 2), ('b', 1), ('c', 0), ('d', 1)]
      let expected = map Just "aabd"
      map (indexWithWeight cards) [0 .. 3] `shouldBe` expected
    it "returns Nothing on index out of bounds" $ do
      indexWithWeight [('a', 3)] 3 `shouldBe` Nothing

  describe "drawCard" $ do
    let run rng state = runStateGen_ rng (\g -> runStateT (drawCard g) state)

    it "returns only card from deck of size 1" $ do
      forM_
        (enumAll :: [Card])
        ( \card -> do
            let deck = fromIndex (fromEnum . (== card))
            fst (run (mkStdGen 1337) deck) `shouldBe` Just card
        )
    it "removes correct card from deck" $ do
      forM_
        [1 .. 50]
        ( \seed -> do
            let deck = allSame 1
            let (maybeCard, newDeck) = run (mkStdGen seed) deck
            maybeCard `shouldSatisfy` isJust
            let card = fromJust maybeCard
            newDeck `shouldBe` fromIndex (fromEnum . (/= card))
        )
    it "return Nothing for empty deck" $ do
      fst (run (mkStdGen 1337) (allSame 0)) `shouldBe` Nothing

testGame :: SpecWith ()
testGame = describe "Game" $ do
  describe "genStartingState" $ do
    let run rng = runStateGen_ rng genStartingState
    let forRandomStates f = forM_ [1 .. 50] (f . run . mkStdGen)

    it "returns correctly sized hands" $ do
      forRandomStates
        ( \state -> do
            length (hands state ! Human) `shouldBe` handSize
            length (hands state ! Computer) `shouldBe` handSize
        )
    it "returns a correctly sized deck" $ do
      forRandomStates
        (\state -> deckSize (deck state) `shouldBe` 50 - 2 * handSize)

  describe "play" $ do
    let run rng state player action = runStateGen_ rng (\g -> runStateT (play player action g) state)
    let basicState =
          GameState
            { hands =
                PlayerVec
                  [CardState (Card Red n) noKnowledge | n <- [One .. Four]]
                  (zipWith (\c n -> CardState (Card c n) noKnowledge) [Red .. Blue] [Two .. Five]),
              piles = ColorVec Nothing Nothing Nothing (Just Four) Nothing,
              deck = allSame 1,
              infoTokens = 3,
              fuseTokens = 2,
              gameEndCountdown = Nothing
            }

    context "when called with Play" $ do
      it "removes the card from hand" $ do
        let firstThreeCards = take 3 . (! Human) . hands
        let lastThreeCards = drop 1 . (! Human) . hands
        (firstThreeCards . snd) (run (mkStdGen 1337) basicState Human (Play 0)) `shouldBe` lastThreeCards basicState
      it "draws a new card" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Play 0)
        length (hands state ! Human) `shouldBe` handSize
        knowledge (last (hands state ! Human)) `shouldBe` noKnowledge
      it "adds the card to the pile if it matches" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Play 0)
        piles state `shouldBe` ColorVec (Just One) Nothing Nothing (Just Four) Nothing
      it "doesn't add the card to the pile if it doesn't match" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Play 1)
        piles state `shouldBe` piles basicState
      it "removes a fuse token if it doesn't match" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Play 1)
        fuseTokens state `shouldBe` fuseTokens basicState - 1
      it "adds an information token if five is played" $ do
        let (_, state) = run (mkStdGen 1337) basicState Computer (Play 3)
        infoTokens state `shouldBe` 4
      it "doesn't add an information token if non-five is played" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Play 0)
        infoTokens state `shouldBe` 3
      it "doesn't add an information token if all are already available" $ do
        let (_, state) = run (mkStdGen 1337) basicState {infoTokens = maxInfoTokens} Computer (Play 3)
        infoTokens state `shouldBe` maxInfoTokens
      it "starts countdown when deck emptied" $ do
        let (_, state) = run (mkStdGen 1337) basicState {deck = fromIndex (fromEnum . (== Card Red One))} Human (Play 0)
        gameEndCountdown state `shouldBe` Just 2

    context "when called with Discard" $ do
      it "removes the card from hand" $ do
        let firstThreeCards = take 3 . (! Human) . hands
        let lastThreeCards = drop 1 . (! Human) . hands
        (firstThreeCards . snd) (run (mkStdGen 1337) basicState Human (Discard 0)) `shouldBe` lastThreeCards basicState
      it "draws a new card" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Discard 0)
        length (hands state ! Human) `shouldBe` handSize
        knowledge (last (hands state ! Human)) `shouldBe` noKnowledge
      it "adds an information token" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Discard 3)
        infoTokens state `shouldBe` 4
      it "doesn't add an information token if all are already available" $ do
        let (_, state) = run (mkStdGen 1337) basicState {infoTokens = maxInfoTokens} Human (Discard 3)
        infoTokens state `shouldBe` maxInfoTokens
      it "leaves piles intact" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Discard 0)
        piles state `shouldBe` ColorVec Nothing Nothing Nothing (Just Four) Nothing
      it "starts countdown when deck emptied" $ do
        let (_, state) = run (mkStdGen 1337) basicState {deck = fromIndex (fromEnum . (== Card Red One))} Human (Discard 0)
        gameEndCountdown state `shouldBe` Just 2

    context "when called with Hint" $ do
      it "changes knowledge for color hint" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Hint (ColorHint Green))
        let noGreen = Knowledge (ColorVec True True False True True) (NumberVec True True True True True)
        let green = Knowledge (ColorVec False False True False False) (NumberVec True True True True True)
        map knowledge (hands state ! Computer) `shouldBe` [noGreen, noGreen, green, noGreen]
      it "changes knowledge for number hint" $ do
        let (_, state) = run (mkStdGen 1337) basicState Human (Hint (NumberHint Two))
        let noTwo = Knowledge (ColorVec True True True True True) (NumberVec True False True True True)
        let two = Knowledge (ColorVec True True True True True) (NumberVec False True False False False)
        map knowledge (hands state ! Computer) `shouldBe` [two, noTwo, noTwo, noTwo]
      it "returns correct action result for color hint" $ do
        let (result, _) = run (mkStdGen 1337) basicState Human (Hint (ColorHint Green))
        result `shouldBe` Hinted (ColorHint Green) [2]
      it "returns correct action result for number hint" $ do
        let (result, _) = run (mkStdGen 1337) basicState Human (Hint (NumberHint Two))
        result `shouldBe` Hinted (NumberHint Two) [0]

  describe "hasGameEnded" $ do
    let basicState =
          GameState
            { hands =
                PlayerVec
                  [CardState (Card Red n) noKnowledge | n <- [One .. Four]]
                  [CardState (Card Blue n) noKnowledge | n <- [One .. Four]],
              piles = allSame Nothing,
              deck = allSame 1,
              infoTokens = 3,
              fuseTokens = 2,
              gameEndCountdown = Nothing
            }

    it "returns false when nothing matches" $ do
      hasGameEnded basicState `shouldBe` False
    it "returns false when countdown ticking" $ do
      hasGameEnded basicState {gameEndCountdown = Just 1} `shouldBe` False
    it "returns true when countdown exhausted" $ do
      hasGameEnded basicState {gameEndCountdown = Just 0} `shouldBe` True
    it "returns true when fuse tokens exhausted" $ do
      hasGameEnded basicState {fuseTokens = 0} `shouldBe` True
    it "returns true when all piles full" $ do
      hasGameEnded basicState {piles = allSame (Just Five)} `shouldBe` True

withPerfectKnowledge :: [Card] -> [CardState]
withPerfectKnowledge = map (\card -> CardState {actual = card, knowledge = Knowledge (fromIndex (== cardColor card)) (fromIndex (== cardNumber card))})

testAi :: SpecWith ()
testAi = describe "Ai" $ do
  it "will play matching card" $ do
    let state =
          GameState
            { hands =
                PlayerVec
                  (withPerfectKnowledge [Card Red number | number <- [One .. Four]])
                  (withPerfectKnowledge [Card Green number | number <- [One .. Four]]),
              piles = ColorVec (Just Five) (Just Five) (Just One) (Just Five) (Just Five),
              deck = allSame 1,
              infoTokens = 4,
              fuseTokens = 3,
              gameEndCountdown = Nothing
            }
    pickAction state `shouldBe` Play 1

  it "will discard unneeded card" $ do
    let state =
          GameState
            { hands =
                PlayerVec
                  (withPerfectKnowledge [Card color Three | color <- [Red .. Blue]])
                  (withPerfectKnowledge [Card color Two | color <- [Red .. Blue]]),
              piles = ColorVec Nothing Nothing (Just Five) Nothing (Just Five),
              deck = CardVec (allSame (NumberVec 1 0 1 1 1)),
              infoTokens = 0,
              fuseTokens = 3,
              gameEndCountdown = Nothing
            }
    pickAction state `shouldBe` Discard 2

  it "will give hint for playable card" $ do
    let state =
          GameState
            { hands =
                PlayerVec
                  (map (\actual -> CardState {actual, knowledge = noKnowledge}) [Card Red One, Card Green Two, Card White Three, Card Yellow Two])
                  (withPerfectKnowledge [Card color Five | color <- [Red .. Blue]]),
              piles = ColorVec (Just Five) (Just One) (Just Five) (Just Five) (Just Five),
              deck = allSame 1,
              infoTokens = 8,
              fuseTokens = 3,
              gameEndCountdown = Nothing
            }
    pickAction state `shouldSatisfy` (\a -> a == Hint (ColorHint Yellow) || a == Hint (NumberHint Two))

main :: IO ()
main = hspec $ do
  testUtils
  testVec
  testCards
  testGame
  testAi
