module Main where

import Test.Hspec (describe, hspec, it, shouldBe)
import Utils (enumerate)

main :: IO ()
main = hspec $ do
  describe "Utils.enumerate" $ do
    it "adds numbers to \"abc\"" $ do
      enumerate "abc" `shouldBe` [(0, 'a'), (1, 'b'), (2, 'c')]
