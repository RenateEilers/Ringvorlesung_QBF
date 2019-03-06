module QBF where

import qualified Data.Set as S
import Data.List 
import Data.Maybe

-- Data declarations
data Problem = Problem {num_atoms :: Int,
                        num_clauses :: Int,
                        qbf :: QBF} deriving (Eq, Show, Ord)

type ID = Int

data Variable = Var {name :: ID} deriving (Eq, Show, Ord)
data Quantifier = Exists {vars :: (S.Set Variable)} | Forall {vars :: (S.Set Variable)} deriving (Eq, Show, Ord)

data Literal = Pos {atom :: Variable} | Neg {atom :: Variable} deriving (Eq, Show, Ord)
type Clause = [Literal]
data QBF = QBF {quantifiers :: (Quantifier,Quantifier),
                clauses :: [Clause]} deriving (Eq, Show, Ord)

-- Function declarations
--fromLiteral :: Literal -> Int
--fromLiteral (Pos v) = name v
--fromLiteral (Neg v) = - name v

toPicosat :: [Clause]-> [[Int]]
toPicosat cs = map (map fromLiteral)  cs
    where   fromLiteral (Pos v) = name v
            fromLiteral (Neg v) = - name v

universalsRight :: QBF -> S.Set Variable
universalsRight form = g $ quantifiers form
    where g (_,Forall v)    = v
          g _               = S.empty

existentialsLeft :: QBF -> S.Set Variable
existentialsLeft form =  g $ quantifiers form
    where   g (Exists v,_)    = v
            g  _              = S.empty

existentials :: QBF -> S.Set Variable
existentials form = g $ quantifiers form
    where   g (Exists v,_)    = v
            g (_,Exists v)    = v
            g _               = S.empty


univerals :: QBF -> S.Set Variable
univerals form = g $ quantifiers form
    where   g (Forall v,_)    = v
            g (_,Forall v)    = v
            g _               = S.empty

-- Functions for clauses

-- Won't work for unit literals; context (clause) is required
genericFindLiterals :: (Literal -> Bool) -> QBF -> [Literal]
genericFindLiterals test form = concatMap (filter test) $ clauses form


findUnitLiterals :: QBF -> [Literal]
findUnitLiterals form = mapMaybe f $ clauses form
    where   e = existentials form
            uR = universalsRight form            
            f cl = let (eC,uC) = partition (flip elem e . atom) cl
                    in if length (nub eC) == 1 && all (flip elem uR . atom) uC then Just (head eC)
                       else Nothing

findPureLiterals :: QBF -> [Literal]
findPureLiterals form = filter f lits
    where   cls = clauses form
            lits = concat cls
            f :: Literal -> Bool
            f (Pos v) = not $ (Neg v) `elem` lits
            f (Neg v) = not $ (Pos v) `elem` lits

           
-- Functions           