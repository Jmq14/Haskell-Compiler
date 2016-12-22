module Main where
	import qualified Parser as Parser
	import qualified Expr as Expr
	import System.Environment
	import System.Exit
	import qualified Data.Map as Map
	import qualified Data.List.Split as Split

	getASTTree s = Parser.myParse s;
	
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
		| otherwise = analyzeArgs (y:xs)

	reConcat :: [String] -> String
	reConcat [] = ""
	reConcat (x:xs) = x ++ (reConcat xs)

	printValueandWork newState newVariable newExpr = do
		print (Expr.valueOfExpr newExpr)
		replWork newExpr newVariable

	replWork preState preVariable = do
		line <- getLine
		let x = Split.splitOn " " line in
			if (x == [])
				then replWork preState preVariable
				else
					if ((head x) == ":q")
						then print "Goodbye"
						else
							if ((head x) == ":i")
								then printValueandWork preVariable preVariable (Parser.myParse( (reConcat (tail x))))
								else do
									print preState
									replWork preState preVariable
	
	mainWork m = 
		if (Map.member "-repl" m)
			then replWork Expr.EmptyExpr Map.empty 
			else print "no"

	main :: IO()
	main = do
--		line <- getLine
		a <- getArgs
		mainWork (analyzeArgs a)
--		print (valueOfExpr (getASTTree line))
