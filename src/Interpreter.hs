module Interpreter where

import Data.Char

data Operator = Plus | Minus | Times | Div
    deriving(Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

data Token = TokenNumber Int
           | TokenOperator Operator
    deriving(Show, Eq)

isSomeDigit :: Char -> Bool
isSomeDigit c = elem c "0123456789"

isSomeOp :: Char -> Bool
isSomeOp c = elem c "+-*/"

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs)
   | isSomeDigit x = TokenNumber (digitToInt x) : tokenize xs
   | isSomeOp x = TokenOperator (operator x) : tokenize xs
   | otherwise = error ("Cannot tokenize " ++ [x])


data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | NumNode Double
  deriving Show

expression :: [Token] -> (Tree, [Token])


term :: [Token] -> (Tree, [Token])


factor :: [Token] -> (Tree, [Token])
