module Evaluate (evaluate) where

import Parser (SyntaxTree (..))

evaluate :: SyntaxTree -> Int
evaluate (Value value) = read value :: Int
evaluate (Operator op left right)
    | op == "+" = (evaluate left) + (evaluate right)
    | op == "-" = (evaluate left) - (evaluate right)
    | op == "*" = (evaluate left) * (evaluate right)
    | op == "/" = (evaluate left) `div` (evaluate right)
    | op == "--" = evaluate left
    | otherwise = evaluate $ Value "Invalid Operator"
