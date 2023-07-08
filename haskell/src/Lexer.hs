module Lexer (tokenize, Token (..)) where

data Token = ValueToken String | OperatorToken String | LParenToken | RParenToken deriving (Show, Eq)

operators :: [String]
operators = ["+", "-", "*", "/", "--", "in", "where"]

tokenize :: [String] -> [Token]
tokenize [] = []
tokenize (arg:args)
    | arg == ")" = (tokenize args) ++ [LParenToken]
    | arg == "(" = (tokenize args) ++ [RParenToken]
    | arg `elem` operators = (tokenize args) ++ [OperatorToken arg]
    | otherwise = (tokenize args) ++ [ValueToken arg]
