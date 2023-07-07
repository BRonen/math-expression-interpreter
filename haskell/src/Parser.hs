module Parser (parse, SyntaxTree (..)) where

import Lexer (Token (..))

data SyntaxTree = Operator String SyntaxTree SyntaxTree | Value String deriving (Show)

parseValue :: [Token] -> SyntaxTree
parseValue ((ValueToken value):(OperatorToken op):tokens) = Operator op (parse tokens) (Value value)
parseValue ((ValueToken value):_) = Value value
parseValue _ = Value "Invalid Value"

getScopeTokens :: [Token] -> Int -> [Token]
getScopeTokens (token:tokens) currentLevel
    | currentLevel == 0 = []
    | token == LParenToken = token:(getScopeTokens tokens (currentLevel + 1))
    | token == RParenToken = token:(getScopeTokens tokens (currentLevel - 1))
    | otherwise = token:(getScopeTokens tokens currentLevel)
getScopeTokens tokens currentLevel
    | currentLevel == 0 = tokens
    | otherwise = []

parseOperator :: [Token] -> SyntaxTree -> SyntaxTree
parseOperator ((OperatorToken op):tokens) carry = Operator op (parse tokens) carry
parseOperator [RParenToken] carry = carry
parseOperator _ carry = carry

parse :: [Token] -> SyntaxTree
parse tokens
    | ValueToken _ <- (head tokens) = parseValue tokens
    | LParenToken <- (head tokens) =
        parseOperator
            (drop (length $ getScopeTokens (tail tokens) 1) (tail tokens))
            (parse $ getScopeTokens (tail tokens) 1)
    | otherwise = Value "Parsing Error"
