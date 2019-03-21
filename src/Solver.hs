module Solver where
import QBF
import Picosat

-------------------------------------------------------------------------------
--                     Data declarations 
-------------------------------------------------------------------------------

data Result = SAT | UNSAT deriving (Show, Eq)
type SolverResult = IO Result

-------------------------------------------------------------------------------
--                      Auxiliary functions
-------------------------------------------------------------------------------

getSolution :: [Clause] -> [Int] -> IO (Result,[Int])
getSolution cls assums =  do
        sol <-  evalScopedPicosat $ do
                    addBaseClauses $ toPicosat cls
                    scopedSolutionWithAssumptions $ assums
        return $ f sol
            where f (Solution s) = (SAT,s)
                  f _            = (UNSAT,[])

negateQBFwithAsgs:: Int -> [Literal] -> [Clause] -> (Int,[Clause])
negateQBFwithAsgs v asgs cls = foldr f (v,[]) cls
    where   f cl (i,cls) = undefined
            

negateClause :: Literal -> Clause -> [Clause]
negateClause freshLit cl = foldr f [] cl
    where f lit cls  = [freshLit,complement lit] : cls

-- For each clause:
--      1) update according to assignment.
--            i.e., if clause contians l that is in assignment, remove clause
--                  if clause contians ~l that is in assignment, remove l
--      2) add fresh var implying not clause (x -> ~C) = ~x || ~C = ~x || (forall c in C) &&~c = 
--      3) add big clause containing all fresh variables 

assignVars :: [Int] -> [Clause] -> [Clause]
assignVars asgs cls = undefined
                


-------------------------------------------------------------------------------
--                      Expansion-based solving
-------------------------------------------------------------------------------

expansionSolve :: Problem -> SolverResult
expansionSolve q = expansionSolveSub q [] []

expansionSolveSub :: Problem -> [Int] -> [Clause] -> SolverResult
expansionSolveSub p alpha psi = do
        let q = clauses $ qbf p
        (resTau,tau) <- getSolution q alpha        
        if resTau == UNSAT
            then return UNSAT 
            else do
                let (vars,notPhiTau) = negateQBFwithAsgs (num_atoms p) (map toLiteral tau) q
                let psi' = psi ++ notPhiTau
                (resAlpha,alpha') <- getSolution psi tau
                if resAlpha == UNSAT
                    then return SAT
                    else do
                        let p' = Problem vars (num_clauses p) (qbf p)
                        expansionSolveSub p' alpha' psi'
        


                        