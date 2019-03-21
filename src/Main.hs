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
    let q = qbf problem    
    putStrLn "Original QBF: "
    print q
    putStr "Pure literals: "
    print $ findPureLiterals $ q
    putStrLn "After pureLiteralElimination: "
    let ple = pureLiteralElimination q
    print ple
    putStr "Unit literals: "
    print  $ findUnitLiterals $ q
    putStrLn "After unit literal elimination: "
    let ule = unitLiteralElimination q
    print ule
    putStrLn "picosat:"
    print $ toPicosat $ clauses ule
    sol <- evalScopedPicosat $ do
            addBaseClauses $ toPicosat $ clauses ule
            scopedSolutionWithAssumptions [-3]
    putStrLn "sol:"
    print sol        

testLoc :: FilePath
testLoc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/mini"

bigLoc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/eval17/2QBF/add20y.sat.qdimacs"