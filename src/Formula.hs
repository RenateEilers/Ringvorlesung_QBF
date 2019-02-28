module Formula where

import Data.Set (Set)

-- Shared datatypes
type Name = String
data Variable = Var {name :: String}
data Quantifier = Exists (Set Variable) | Forall (Set Variable)

-- FOL formulas
data FOL_Formula = QFormula Quantifier FOL_Formula 
            | Not FOL_Formula
            | Or FOL_Formula FOL_Formula
            | And FOL_Formula FOL_Formula
            | Impl FOL_Formula FOL_Formula
            | Atom Variable



-- Formulas in CNF
data Literal = Pos Variable | Neg Variable
type Clause = (Set Literal)
data CNF_Formula = CNF [Quantifier] (Set Clause)

