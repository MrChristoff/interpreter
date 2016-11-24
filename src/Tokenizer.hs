module Tokenizer where

data Operator = Plus | Minus | Times | Div
    deriving(Show, Eq)

data Parens = Open | Close
    deriving(Show, Eq)

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div

data Token = TokenNumber Double
           | TokenOperator Operator
           | TokEnd
           | TokenParens Parens
    deriving(Show, Eq)

isSomeDigit :: Char -> Bool
isSomeDigit c = elem c "0123456789"

isSomeOp :: Char -> Bool
isSomeOp c = elem c "+-*/"

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs)
   | isSomeDigit x = TokenNumber (read [x]::Double) : tokenize xs
   | isSomeOp x = TokenOperator (operator x) : tokenize xs
   | otherwise = error ("Cannot tokenize " ++ [x])
