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

parens :: Char -> Parens
parens c | c == '(' = Open
         | c == ')' = Close

data Token = TokenNumber Double
           | TokenOperator Operator
           | TokEnd
           | TokenParens Parens
    deriving(Show, Eq)

isSomeDigit :: Char -> Bool
isSomeDigit c = elem c "0123456789"

isSomeOp :: Char -> Bool
isSomeOp c = elem c "+-*/"

isSomeParens :: Char -> Bool
isSomeParens c = elem c "()"

fullNumber :: Char -> String -> [Token]
fullNumber x xs =
   let (digit, xs') = span isSomeDigit xs in
   TokenNumber (read (x : digit)::Double) : tokenize xs'

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs)
   | isSomeParens x = TokenParens (parens x) : tokenize xs
   | isSomeDigit x = fullNumber x xs
   | isSomeOp x = TokenOperator (operator x) : tokenize xs
   | otherwise = error ("Cannot tokenize " ++ [x])
