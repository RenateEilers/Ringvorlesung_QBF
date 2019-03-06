{
module Parser where

import QBF
import qualified Lexer as L
import qualified Data.Set as Set 

}
%name parseQDIMACS
%tokentype {L.Token}
-- %monad { Maybe } { >>= } { return }
%error {parseError}

%token
p       {L.TokenP}
cnf     {L.TokenCNF}
'0'     {L.TokenZero}
'-'     {L.TokenMinus}
a       {L.TokenA}
e       {L.TokenE}
int     {L.TokenInt $$}

%%
--Input : p cnf int '0''-' a e {[]}

Problem : p cnf int int QBF                     {Problem $3 $4 $5}

QBF : QuantifierSet QuantifierSet ClauseSet    {QBF ($1,$2) $3}

--Prefix : Rev_Prefix                             {reverse $1}

--Rev_Prefix : QuantifierSet                      {[$1]}
--        | Rev_Prefix QuantifierSet              {$2:$1}

QuantifierSet : a AtomSet                    {Forall $2}
            | e AtomSet                      {Exists $2}
             --| p cnf int '0''-' a e {Exists Set.empty}
 
ClauseSet : Clause                              {[$1]}
        | Clause ClauseSet                     {$1 : $2}

Clause : '0'                                    {[]}
    | Literal Clause                            {$1 : $2}

AtomSet : '0'                                   {Set.empty}
        | Atom AtomSet                          {Set.insert $1 $2}

Atom : int                                      {Var $1}

Literal : Atom                                  {Pos $1}
        | '-' Atom                              {Neg $2}

----



{
parseError :: [L.Token] -> a
parseError t = error $ "Parse error" ++ show t

    --main = getContents >>= print . L.lexer
}