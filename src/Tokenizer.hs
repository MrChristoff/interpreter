module Tokenizer where

data Operator = Plus | Minus | Times | Div
    deriving(Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

data Token = TokenNumber Double
           | TokenOperator Operator
           | TokEnd
    deriving(Show, Eq)

isSomeDigit :: Char -> Bool
isSomeDigit c = elem c "0123456789"

isSomeOp :: Char -> Bool
isSomeOp c = elem c "+-*/"

fullNumber :: Char -> String -> [Token]
fullNumber x xs =
   let (digit, xs') = span isSomeDigit xs in
   TokenNumber (read (x : digit)::Double) : tokenize xs'

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs)
   | isSomeDigit x = fullNumber x xs
   | isSomeOp x = TokenOperator (operator x) : tokenize xs
   | otherwise = error ("Cannot tokenize " ++ [x])
