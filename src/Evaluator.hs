module Evaluator where

import Parser
import Tokenizer

evaluateTree :: Tree -> Double
evaluateTree (NumNode x) = x
evaluateTree (SumNode op leftTree rightTree) = 
    let left = evaluateTree leftTree
        right = evaluateTree rightTree
    in
        case op of
           Plus -> left + right
           Minus -> left - right

evaluateTree (ProdNode op leftTree rightTree) =
    let left = evaluateTree leftTree
        right = evaluateTree rightTree
    in
        case op of
           Times -> left * right
           Div -> left / right