module Main (main) where

data SyntaxTree = Tree String SyntaxTree SyntaxTree | Leaf String | Any [Token] deriving (Show)

data Token = Value String | Operator String deriving (Show, Eq)

operators :: [String]
operators = ["+", "-", "*", "/"]

evaluate :: SyntaxTree -> Int
evaluate (Leaf value) = read value :: Int
evaluate (Tree op left right)
    | op == "+" = (evaluate left) + (evaluate right)
    | op == "-" = (evaluate left) - (evaluate right)
    | op == "*" = (evaluate left) * (evaluate right)
    | op == "/" = (evaluate left) `div` (evaluate right)
    | otherwise = evaluate $ Leaf "0"

parseOperator :: [Token] -> SyntaxTree
parseOperator [] = Leaf "Empty"
parseOperator ((Value left):(Operator op):tokens) = Tree op (Leaf left) (parse tokens)

parseValue :: [Token] -> SyntaxTree
parseValue [] = Leaf "Empty"
parseValue [(Value value)] = Leaf value
parseValue ((Value value):tokens) = parseOperator $ [Value value] ++ tokens

parse :: [Token] -> SyntaxTree
parse [] = Leaf "Empty"
parse tokens
    | Value _ <- (head tokens) = parseValue tokens
    | Operator _ <- (head tokens) = parseOperator tokens

tokenize :: [String] -> [Token]
tokenize [] = []
tokenize (arg:args) =
    if (arg `elem` operators) then
        [Operator arg] ++ (tokenize args)
    else
        [Value arg] ++ (tokenize args)

main :: IO ()
main = do
    fileContent <- readFile "input.mock"
    let tokens = tokenize . words $ fileContent
    print tokens
    print . parse $ tokens
    print . evaluate . parse $ tokens