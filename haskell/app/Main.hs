module Main (main) where

data SyntaxTree = Operator String SyntaxTree SyntaxTree | Value String deriving (Show)

data Token = ValueToken String | OperatorToken String | LParenToken | RParenToken deriving (Show, Eq)

operators :: [String]
operators = ["+", "-", "*", "/", "--"]

evaluate :: SyntaxTree -> Int
evaluate (Value value) = read value :: Int
evaluate (Operator op left right)
    | op == "+" = (evaluate left) + (evaluate right)
    | op == "-" = (evaluate left) - (evaluate right)
    | op == "*" = (evaluate left) * (evaluate right)
    | op == "/" = (evaluate left) `div` (evaluate right)
    | op == "--" = evaluate left
    | otherwise = evaluate $ Value "Invalid Operator"

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

tokenize :: [String] -> [Token]
tokenize [] = []
tokenize (arg:args)
    | arg == ")" = (tokenize args) ++ [LParenToken]
    | arg == "(" = (tokenize args) ++ [RParenToken]
    | arg `elem` operators = (tokenize args) ++ [OperatorToken arg]
    | otherwise = (tokenize args) ++ [ValueToken arg]

main :: IO ()
main = do
    fileContent <- readFile "input.example"
    let tokens = tokenize . words $ fileContent
    print . parse $ tokens
    print . evaluate . parse $ tokens