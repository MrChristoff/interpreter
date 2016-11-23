module Main where

import Interpreter
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "tokenize" $ do
    it "returns a token when passed a string" $ do
      tokenize "1" `shouldBe` [TokenNumber 1]
