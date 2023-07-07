module Main (main) where

import Lexer (tokenize)
import Parser (parse)
import Evaluate (evaluate)

main :: IO ()
main = do
    fileContent <- readFile "input.example"
    let tokens = tokenize . words $ fileContent
    print . parse $ tokens
    print . evaluate . parse $ tokens