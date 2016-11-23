module Main where

import Interpreter
import Test.Hspec
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "tokenize" $ do
    it "returns a token when passed a string" $ do
      tokenize "1" `shouldBe` [TokenNumber 1]
    it "returns a op token when passed a string of operator" $ do
      tokenize "+" `shouldBe` [TokenOperator Plus]
    it "returns an exception when unknown token is passed" $ do
      evaluate (tokenize "d") `shouldThrow` anyErrorCall

  describe "parse" $ do
    it "should return a parse tree of tokens" $ do
      let x = tokenize "1"
      parse x `shouldBe` [NumNode 1.0]
    it "should return a parse tree of tokens" $ do
      let x = tokenize "1+1"
      parse x `shouldBe` [SumNode Plus NumNode 1.0 NumNode 1.0]
    it "should return a parse tree of tokens" $ do
      let x = tokenize "1*1+1"
      parse x `shouldBe` [SumNode Plus NumNode 1.0 ProdNode Times NumNode 1.0 NumNode 1.0]
