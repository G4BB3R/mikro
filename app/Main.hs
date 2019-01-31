module Main where

import qualified Parser 
import System.Exit

main :: IO ()
main = do
    putStrLn "1. Starting Parser"

    source <- readFile "source.mk"

    let result = Parser.run source

    case result of 
        Left err -> do
            putStrLn "- Parser Failed"
            print err
            die "Aborted.\n"

        Right ok -> do
            putStrLn "- Parser ok."
            print ok


    putStrLn "Finished."

