module Main (main) where

import System.Environment (getArgs)

import Lexer (tokenize)
import Parser (parse)
import Evaluate (evaluate)

main :: IO ()
main = do
    args <- getArgs

    if length args == 0 then do
        print "Need to specify a filename/path to be used as input:"
        putStrLn $ "\t./[bin name] [filename/path]"
    else do
        let filePath = head args
        fileContent <- readFile filePath

        let tokens = tokenize . words $ fileContent
        print $ evaluate (parse tokens) []