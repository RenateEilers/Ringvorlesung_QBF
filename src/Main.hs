module Main where

import qualified Lexer as L
import Parser
import System.Environment
import System.Exit
import QBF
import Picosat
import Solver

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
    putStrLn "Problem: "
    print problem
    --putStr "Pure literals: "
    --print $ findPureLiterals $ q
    --putStrLn "After pureLiteralElimination: "
    --let ple = pureLiteralElimination q
    --print ple
    --putStr "Unit literals: "
    --print  $ findUnitLiterals $ q
    --putStrLn "After unit literal elimination: "
    --let ule = unitLiteralElimination q
    --print ule
    --putStrLn "picosat:"
    --print $ toPicosat $ clauses ule
    --sol <- evalScopedPicosat $ do
    --        addBaseClauses $ toPicosat $ clauses ule
    --        scopedSolutionWithAssumptions [-3]
    --putStrLn "sol:"
    --print sol        
    putStrLn "solver:"
    sol2 <- expansionSolve problem
    print sol2


miniLoc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/mini"
mini1Loc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/test1.qdimacs"
mini2Loc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/mini2"
mini3Loc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/mini3"
mini4Loc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/mini4"
mini5Loc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/mini5"

medLoc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/116_SAT.qdimacs"
med2Loc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/med2.qdimacs"
med3Loc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/test2.qdimacs"
med4Loc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/16966_UNSAT.qdimacs"
med5Loc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/small/1_SAT.qdimacs"

bigLoc = "/mnt/win/Documenten/TUW/Ringvorlesung/QBF/test/eval17/2QBF/add20y.sat.qdimacs"