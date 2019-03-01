module QBF where
	import Data.Set (Set)

	type Name = Int
	data Variable = Var {name :: Name}
	data Quantifier = Exists (Set Variable) | Forall (Set Variable)

	data Literal = Pos Variable | Neg Variable
	type Clause = (Set Literal)
	data QBF = CNF [Quantifier] (Set Clause)