module Main where

import Tokenizer
import Parser
import Evaluator
import Interpreter
import Test.Hspec
import Control.Exception (evaluate)


main :: IO ()
main = hspec $ do
  describe "tokenize" $ do
    it "returns an empty list when passed an empty list" $ do
      tokenize "" `shouldBe` []
    it "returns a token when passed a string" $ do
      tokenize "1" `shouldBe` [TokenNumber 1.0]
    it "returns a op token when passed a string of operator" $ do
      tokenize "+" `shouldBe` [TokenOperator Plus]
    it "returns multiple digit tokens" $ do
      tokenize "123" `shouldBe` [TokenNumber 123.0]
    it "returns an exception when unknown token is passed" $ do
      evaluate (tokenize "d") `shouldThrow` anyErrorCall

  describe "isSomeDigit" $ do
    it "should return true when passed a digit" $ do
      isSomeDigit '1' `shouldBe` True
    it "should return false when passed anything other than a digit" $ do
      isSomeDigit 'f' `shouldBe` False

  describe "fullNumber" $ do
    it "should turn list of numbers into multi digit number" $ do 
      fullNumber '1' "23" `shouldBe` [TokenNumber 123.0]

  describe "isSomeOp" $ do
    it "should return true when passed an operator" $ do
      isSomeOp '+' `shouldBe` True
    it "should return false when passed anything other than an operator" $ do
      isSomeOp '3' `shouldBe` False

  describe "operator" $ do
    it "should return the data type 'Plus' when passed '+'" $ do
      operator '+' `shouldBe` Plus
    it "should return the data type 'Minus' when passed '-'" $ do
      operator '-' `shouldBe` Minus
    it "should return the data type 'Times' when passed '*'" $ do
      operator '*' `shouldBe` Times
    it "should return the data type 'Div' when passed '/'" $ do
      operator '/' `shouldBe` Div

  describe "lookAhead" $ do
    it "returns an empty list when passed an empty list" $ do
      lookAhead [] `shouldBe` TokEnd
    it "should return the head of the list it is passed" $ do
      lookAhead [TokenNumber 1.0, TokenNumber 3.0, TokenOperator Plus] `shouldBe` TokenNumber 1.0

  describe "accept" $ do
    it "returns an empty list when passed an empty list" $ do
      evaluate (accept []) `shouldThrow` errorCall "Nothing to accept"
    it "should return the tail of the list it is passed" $ do
      accept [TokenNumber 1.0, TokenNumber 3.0, TokenOperator Plus] `shouldBe` [TokenNumber 3.0, TokenOperator Plus]

  describe "factor" $ do
    it "returns a tree and a token list given a token list" $ do
      factor [TokenNumber 1.0] `shouldBe` (NumNode 1.0, [])
      factor [TokenNumber 1.0, TokenNumber 6.0] `shouldBe` (NumNode 1.0, [TokenNumber 6.0])

  describe "term" $ do
    it "returns a tree and a token list given a token list" $ do
      term [TokenNumber 1.0] `shouldBe` (NumNode 1.0, [])
      term [TokenNumber 6.0, TokenOperator Div, TokenNumber 6.0] `shouldBe` (ProdNode Div (NumNode 6.0) (NumNode 6.0), [])
      term [TokenNumber 6.0, TokenOperator Times, TokenNumber 6.0] `shouldBe` (ProdNode Times (NumNode 6.0) (NumNode 6.0), [])
      term [TokenNumber 1.0, TokenOperator Plus, TokenNumber 1.0] `shouldBe` (NumNode 1.0,[TokenOperator Plus,TokenNumber 1.0])

  describe "expression" $ do
    it "returns a tree and a token list given a token list" $ do
      expression [TokenNumber 1.0] `shouldBe` (NumNode 1.0, [])
      expression [TokenNumber 1.0, TokenOperator Plus, TokenNumber 1.0] `shouldBe` (SumNode Plus (NumNode 1.0) (NumNode 1.0), [])
      expression [TokenNumber 1.0, TokenOperator Minus, TokenNumber 1.0] `shouldBe` (SumNode Minus (NumNode 1.0) (NumNode 1.0), [])
      expression [TokenNumber 6.0, TokenOperator Div, TokenNumber 6.0] `shouldBe` (ProdNode Div (NumNode 6.0) (NumNode 6.0), [])

  describe "parse" $ do
    it "should return a parse tree of tokens" $ do
      parse [TokenNumber 1.0] `shouldBe` NumNode 1.0
      -- 1+1
      parse [TokenNumber 1.0, TokenOperator Plus, TokenNumber 1.0] `shouldBe` SumNode Plus (NumNode 1.0) (NumNode 1.0)
      -- 1+1*1
      parse [TokenNumber 1.0, TokenOperator Plus, TokenNumber 1.0, TokenOperator Times, TokenNumber 1.0] `shouldBe` SumNode Plus (NumNode 1.0) (ProdNode Times (NumNode 1.0) (NumNode 1.0))

  describe "evaluate" $ do
    it "should return a Double when given a parse tree" $ do
      evaluateTree (NumNode 1.0) `shouldBe` 1.0
      -- 1+1
      evaluateTree (SumNode Plus (NumNode 1.0) (NumNode 1.0)) `shouldBe` 2.0
      -- 1+1*2
      evaluateTree (SumNode Plus (NumNode 1.0) (ProdNode Times (NumNode 2.0) (NumNode 1.0))) `shouldBe` 3.0
      -- 6-2/2
      evaluateTree (SumNode Minus (NumNode 6.0) (ProdNode Div (NumNode 2.0) (NumNode 2.0))) `shouldBe` 5.0

  describe "interpret" $ do
    it "should return a Double when given a string" $ do
      interpret "1+3*6/2-1" `shouldBe` 9.0
      interpret "10+30*10/2-1" `shouldBe` 159.0
