
{
module Lexer (main) where
}

%wrapper "basic"

$digit = [0-9]			-- digits
$nonZeroDigit = [1-9]	-- digits > 0
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-	
	$white+		;
	c.* 		; -- Comment line
	p 			{\s -> TokenP}
	cnf 		{\s -> TokenCNF}
	e 			{\s -> TokenE}
	a 			{\s -> TokenA}
	"0" 		{\s -> TokenZero}
	"-" 		{\s -> TokenMinus}
	$nonZeroDigit $digit*  	{\s -> Int (read s)}


{
	
data Token = TokenP
			| TokenCNF
			| TokenE			
			| TokenA
			| TokenZero
			| TokenMinus
			| Int Int
			deriving (Eq, Show)

main =  do
	s <- getContents
	print (alexScanTokens s)
}