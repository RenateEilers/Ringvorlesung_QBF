{
    module Parser where

    import Data.Set (Set)
    import QDIMACS
    import QBF
    import qualified Lexer as L
}
%name QDIMACParser
%lexer{L.lexer}
%tokentype {L.Token}
%error {parseError}

%token
p       {L.TokenP}
cnf     {L.TokenCNF}
int     {L.TokenInt $$}
'0'     {L.TokenZero}
'-'     {L.TokenMinus}
a       {L.TokenA}
e       {L.TokenE}

%%

Input : Problem Prefix ClauseList               {Input $1 $2 $3}

Problem : p cnf int int                         {Problem $3 $4} 
    
Prefix : QuantifierSets                         {$1}

QuantifierSets : QuantifierSet                   {[$1]}
                | QuantifierSets QuantifierSet   {$2:$1}

QuantifierSet : a AtomSet '0'                   {Forall $2}
            | e AtomSet '0'                     {Exists $2}

AtomSet : Atom                                  {Set.singleton $1}
        | AtomSet Atom                          {Set.insert $2 $1}

Atom : int                                      {$1}

ClauseList : ClauseList Clause                  {$2:$1}
        | Clause                                {[$1]}

Clause : Literal                               {Set.singleton $1}
        | Clause Literal                       {Set.insert $2 $1}

Literal : Atom                                  {Pos $1}
        | '-' Atom                              {Neg $2}

----



{
    parseError :: [Token] -> a
    parseError _ = error "Parse error"

    parse = getContents >>= print . L.lexer
}