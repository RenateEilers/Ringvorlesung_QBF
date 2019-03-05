module QBF where

import Data.Set (Set)

-- Data declarations
data Problem = Problem {num_atoms :: Int,
                        num_clauses :: Int,
                        qbf :: QBF} deriving (Eq, Show, Ord)

type ID = Int

data Variable = Var {name :: ID} deriving (Eq, Show, Ord)
data Quantifier = Exists (Set Variable) | Forall (Set Variable) deriving (Eq, Show, Ord)

data Literal = Pos Variable | Neg Variable deriving (Eq, Show, Ord)
type Clause = (Set Literal)
data QBF = QBF [Quantifier] (Set Clause) deriving (Eq, Show, Ord)