module Main where
	import Parser
	import Expr
	import System.Environment
	import System.Exit
	import qualified Data.Map as Map

	getASTTree s = Parser.myParse s;
	
	analyzeArgs :: [String] -> Data.Map
	analyzeArgs (x:[]) = 
		if (x == "-repl")
			then Map.insert "-repl" "233" Map.empty
			else Map.empty
	analyzeArgs [] = Map.empty
	analyzeArgs (x:(y:xs))
		| x == "-i" = Map.insert "-i" y (analyzeArgs xs)
		| x == "-t" = Map.insert "-t" y (analyzeArgs xs)
		| x == "-o" = Map.insert "-o" y (analyzeArgs xs)
		| x == "-repl" = Map.insert "-repl" "233" (analyzeArgs (y:xs))

	main :: IO()
	main = do
--		line <- getLine
		a <- getArgs
		args <- analyzeArgs a
		print parse
--		print (valueOfExpr (getASTTree line))
