module Interpreter where

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

tokenize :: String -> [Token]
tokenize [] = []
tokenize (x : xs)
   | isSomeDigit x = TokenNumber (read [x]::Double) : tokenize xs
   | isSomeOp x = TokenOperator (operator x) : tokenize xs
   | otherwise = error ("Cannot tokenize " ++ [x])


data Tree = SumNode Operator Tree Tree
          | ProdNode Operator Tree Tree
          | NumNode Double
  deriving (Show, Eq)

lookAhead :: [Token] -> Token
lookAhead [] = TokEnd
lookAhead (x : xs) = x

accept :: [Token] -> [Token]
accept [] = error "Nothing to accept"
accept (x : xs) = xs

expression :: [Token] -> (Tree, [Token])
expression tokens =
   let (termTree, tokens') = term tokens
   in
      case lookAhead tokens' of
         (TokenOperator op) | elem op [Plus, Minus] ->
            let (expressionTree, tokens'') = expression (accept tokens')
            in (SumNode op termTree expressionTree, tokens'')
         _ -> (termTree, tokens')

term :: [Token] -> (Tree, [Token])
term tokens =
  let (factorTree, tokens') = factor tokens
  in
     case lookAhead tokens' of
       (TokenOperator op) | elem op [Times, Div] ->
          let (termTree, tokens'') = term (accept tokens')
          in (ProdNode op factorTree termTree, tokens'')
       _ -> (factorTree, tokens')

factor :: [Token] -> (Tree, [Token])
factor tokens =
  case lookAhead tokens of
    (TokenNumber int) -> (NumNode int, accept tokens)
    _ -> error $ "Parse error on token: " ++ show tokens

parse :: [Token] -> Tree
parse tokens = let (tree, tokens') = expression tokens
               in
                 if null tokens'
                 then tree
                 else error $ "Leftover tokens: " ++ show tokens'
