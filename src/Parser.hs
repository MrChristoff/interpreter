module Parser where

import Tokenizer

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
    (TokenParens Open) ->
      let (expressionTree, tokens') = expression (accept tokens)
      in
         if lookAhead tokens' /= TokenParens Close
         then error $ "Missing closing bracket"
         else (expressionTree, accept tokens')
    _ -> error $ "Parse error on token: " ++ show tokens

parse :: [Token] -> Tree
parse tokens = let (tree, tokens') = expression tokens
               in
                 if null tokens'
                 then tree
                 else error $ "Leftover tokens: " ++ show tokens'
