module Main where

import qualified Lexer as L
import Parser
import System.Environment
import System.Exit

main = do
    args <- getArgs
    print args

--parse ["-h"] = putStrLn "Usage: NAME [-vh] <file>" >> exitWith ExitSuccess
--parse ["-v"] = putStrLn "NAME 0.1" >> exitWith ExitSuccess
--parse [] =  getContents 
--parse (file:[]) = readFile file exitWith ExitSuccess
--parse _  = parse ["-h"]

test path = do
    input <- readFile path
    print $ L.alexScanTokens input
    print $ parseQDIMACS $ L.alexScanTokens input

testLoc :: FilePath
testLoc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/mini"

