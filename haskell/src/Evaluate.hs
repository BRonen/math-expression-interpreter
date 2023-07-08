module Evaluate (evaluate) where

import Debug.Trace

import Parser (SyntaxTree (..))

extractKeyValue :: [Char] -> ([Char], [Char])
extractKeyValue string = (fst variable, tail $ snd variable)
    where variable = break (=='=') string

hasKeyInScope :: [Char] -> [([Char], [Char])] -> Maybe [Char]
hasKeyInScope label [] = Nothing
hasKeyInScope label ((key, value):keys)
    | label == key = Just value
    | otherwise = hasKeyInScope label keys

evaluateVariable :: SyntaxTree -> [([Char], [Char])] -> Int
evaluateVariable (Operator op left right) variables = case left of
    Value variable -> evaluate right ((extractKeyValue variable):variables)
    _ -> evaluate (Value "Invalid variable operator") variables

evaluate :: SyntaxTree -> [([Char], [Char])] -> Int
evaluate (Value value) variables = case (hasKeyInScope value variables) of
    Just foundValue -> read foundValue :: Int
    Nothing -> read value :: Int
evaluate (Operator op left right) variables
    | op == "+" = (evaluate left variables) + (evaluate right variables)
    | op == "-" = (evaluate left variables) - (evaluate right variables)
    | op == "*" = (evaluate left variables) * (evaluate right variables)
    | op == "/" = (evaluate left variables) `div` (evaluate right variables)
    | op == "--" = trace (show right) (evaluate left variables)
    | op == "in" = evaluateVariable (Operator op left right) variables
    | op == "where" = evaluateVariable (Operator op right left) variables
    | otherwise = evaluate (Value "Invalid Operator") variables
