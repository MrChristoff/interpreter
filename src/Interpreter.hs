module Interpreter where

import Tokenizer
import Parser
import Evaluator

interpret :: String -> Double
interpret x = evaluateTree (parse (tokenize x))