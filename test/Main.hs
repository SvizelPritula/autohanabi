module Main where

import Control.Exception (evaluate)
import Test.Hspec (SpecWith, anyException, describe, hspec, it, shouldBe, shouldSatisfy, shouldThrow)
import Utils (enumerate, indexOf, infinity, removeNth)

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
    it "can remove the first element" $ do
      removeNth 0 "abc" `shouldBe` ('a', "bc")
    it "can remove an element from the middle" $ do
      removeNth 2 "abcde" `shouldBe` ('c', "abde")
    it "can remove the last element" $ do
      removeNth 2 "abc" `shouldBe` ('c', "ab")
    it "throws an exception when index is out of bounds" $ do
      evaluate (removeNth 3 "abc") `shouldThrow` anyException

  describe "infinity" $ do
    it "is very large" $ do
      infinity `shouldSatisfy` (>= 1e300)

main :: IO ()
main = hspec $ do
  testUtils
