module Formulas where

import           Data.List.Split
import           Text.Regex.PCRE

-- For modelling our GCL dialect. The report states clearly in how we differ
-- from the usual GCL constructs. 

-- Type synonyms for readability, not about constraining possible programs.
type ProgramName = String
type Parameters = [Variable]
type Name = String
type Inv = Exp

-- The "top level" definition for a program. As we did not implement actual
-- function calls, the programName and Parameters are not used.
data Program = Program ProgramName Parameters Stmt deriving (Show, Eq)
-- These are the standard GCL constructs, but without Return as a statement.
-- Also note LV stands for "let variables", though is used for both variable
-- and function declarations used in the program.
data Stmt = Skip
    | Assert Exp -- Also used for post-conditions.
    | Assume Exp -- Also used for pre-conditions.
    | Exp := Exp -- Assignment.
    | Seq Stmt Stmt -- Sequentual , ";"
    | Stmt :<>: Stmt -- Nondetchoice, Statement <Block> Statement
    | While Inv Exp Stmt
    | LV [Variable] [FuncDecl] Stmt deriving (Show, Eq)

data Variable = Var Name Type deriving (Show, Eq)
-- Function declarations are of this form:
-- foo:int,bool|int
-- The last Type is the returntype.
data FuncDecl = FuncDecl Name [Type] Type deriving (Show, Eq)
data BoundVariable = BVar Name Type   deriving (Show, Eq)

data Exp = I Int
    | T -- True
    | F -- False
    | Name Name -- Bare variable.
    | Exp :+: Exp   -- Plus
    | Exp :-: Exp   -- Minus
    | Exp :*: Exp   -- Times
    | Exp :/: Exp   -- Divide
    | Exp :||: Exp  -- Short circuit or
    | Exp :&&: Exp  -- Short circuit and
    | Exp :/\: Exp  -- And
    | Exp :\/: Exp  -- Or
    | Exp :==: Exp  -- Equals
    | Exp :>: Exp   -- GT
    | Exp :<: Exp   -- LT
    | Exp :>=: Exp  -- GEQ
    | Exp :<=: Exp  -- LEQ
    | Exp :->: Exp  -- Implies
    | Neg Exp
    | ITE Exp Exp Exp -- If-then-else
    | Func Name [Exp] -- For function calls.
    | Forall [BoundVariable] Exp -- Allowing multiple bounded variables
    | A Name Exp deriving (Show, Eq) -- Array Name[Exp]


data Type = TPrim PrimitiveType | TArr PrimitiveType deriving (Show, Eq)
data PrimitiveType = PrimInt | PrimBool | PrimUnknown deriving (Show, Eq)


{-
 -           STATEMENT CONSTRUCTION
 -}

readType :: String -> Type
readType string = case string of
    "int"    -> TPrim PrimInt
    "bool"   -> TPrim PrimBool
    "[int]"  -> TArr PrimInt
    "[bool]" -> TArr PrimBool
    _    -> error "not a correct type!"

-- Mimicing the "let .. in .."
llet :: String -> String -> [Stmt] -> Stmt
llet varDecs funcDecs stmts = LV (readVarDecs varDecs) (readFuncDecs funcDecs) (chainStmts stmts)

-- In our DSL we treat [Stmt] like Stmt;Stmt, but in the datastructure we want a
-- dedicated data structure. Stop chaining when encountering an assignment to 
-- the return variable. As our post-condition is included in our statements for 
-- convenience, don't forget that one.
chainStmts :: [Stmt] -> Stmt
chainStmts stmts = case stmts of
    [] -> Skip
    [x] -> x
    ((Name "return" := e):xs) -> Seq (Name "return" := e) (last xs)
    (x:xs)      -> Seq x (chainStmts xs)

-- For example: "x:bool,y:[int]"
readVarDecs :: String -> [Variable]
readVarDecs [] = []
readVarDecs string = map (\[v,t] -> Var v (readType t)) splitList
    where
        varList = splitOn "," string
        splitList =  map (splitOn ":") varList

-- For example: "foo(int,bool|int);bar(bool|bool)".
readFuncDecs :: String -> [FuncDecl]
readFuncDecs string = map (\(_:n:args:returnType:_) -> FuncDecl n (parseArgs args) (readType returnType)) groupList
        where
            funcList = splitOn ";" string
            pattern = "([a-z]+):((?:[a-z]+,)*[a-z]+)\\|([a-z]+)"
            -- Cast to String the captured groups.
            groupList :: [[String]]
            groupList = concatMap (=~ pattern) funcList
            -- For a single list of variables (the input vars for a function)
            parseArgs args = map readType (splitOn "," args)

assume :: Exp -> Stmt
assume = Assume

assert :: Exp -> Stmt
assert = Assert

inv :: Exp -> Inv
inv e = e

while :: Inv -> Exp -> [Stmt] -> Stmt
while inv exp stmts = While inv exp (chainStmts stmts)

sequence :: Stmt -> Stmt -> Stmt
sequence = Seq


{-
 -           EXPRESSION CONSTRUCTION
 -}

i :: Int -> Exp
i = I

neg :: Exp -> Exp
neg = Neg

func :: Name -> [Exp] -> Exp
func = Func

ref :: String -> Exp
ref = Name

-- array name [b]
array :: Name -> [Exp] -> Exp
array n [e] = A n e
array _ _ = error "invalid array construction"
