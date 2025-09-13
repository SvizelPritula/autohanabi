module Main where

import Control.Exception (evaluate)
import Test.Hspec (SpecWith, anyException, describe, hspec, it, shouldBe, shouldSatisfy, shouldThrow)
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
      set B "x" (TestVec "a" "b" "c") `shouldBe` TestVec "a" "s" "c"

  describe "vmapWithKey" $ do
    it "maps everything correctly" $ do
      vmapWithKey (,) (TestVec 'a' 'b' 'c') `shouldBe` TestVec (A, 'a') (B, 'b') (C, 'c')

  describe "toList" $ do
    it "returns all elements" $ do
      toList (TestVec 'a' 'b' 'c') `shouldBe` "abc"

main :: IO ()
main = hspec $ do
  testUtils
