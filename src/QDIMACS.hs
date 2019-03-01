module QDIMACS where

import Data.Set (Set)
import QBF

data Input = Input {problem :: Problem,
                    prefix :: Prefix,
                    clauses :: [Clause]}

data Problem = Problem {atoms :: Int,
                        clauses :: Int}

type Prefix = [Quantifier]

