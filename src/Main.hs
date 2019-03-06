module Main where

import qualified Lexer as L
import Parser
import System.Environment
import System.Exit
import QBF
import Picosat

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
    let problem = parseQDIMACS $ L.alexScanTokens input
    solution <- solve $ toPicosat $ clauses $ qbf problem
    print  $ findUnitLiterals $ qbf problem
    print $ findPureLiterals $ qbf problem
    --print units
    --print problem
    --print solution

testLoc :: FilePath
testLoc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/mini"

