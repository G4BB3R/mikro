module Main where

import qualified Parser 
import qualified CodeGen
import System.Exit
import qualified Data.Either 

main :: IO ()
main = do
    putStrLn "1. Starting lexical and syntactic analysis\n"

    source <- readFile "source.mk"

    let result = Parser.run source

    case result of 
        Left err -> do
            print err
            die "Aborted.\n"

        _ -> return ()
            


    let ast = head $ Data.Either.rights $ [result]
    print ast

    putStrLn "\n\n2. Starting semantic analysis\n"
    putStrLn "[TODO!]\n\n"

    putStrLn "3. Starting code generation\n"
    let output = CodeGen.run ast
    putStrLn output
    writeFile "source.tmp.js" output
