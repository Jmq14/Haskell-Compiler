module Main where
	import qualified Parser as Parser
	import qualified Expr as Expr
	import qualified Tree as Tree
	import qualified Variable as Variable
	import qualified ParseStatement as ParseStatement
	import qualified ParseExpr as ParseExpr

	import System.Environment
	import System.Exit

	import qualified Data.Map as Map
	import qualified Data.List.Split as Split
	import qualified Data.Ratio as Ratio

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
	reConcat (x:xs) = x ++ " " ++ (reConcat xs)

	printValueandWork state variable output = do
		print output
		replWork state variable

	replWork state variable = do
		line <- getLine
		let x = Split.splitOn " " line in
			if (x == [])
				then replWork state variable
				else
					if ((head x) == ":q")
						then print "Goodbye"
						else
							if ((head x) == ":i")
								then do
									--print (ParseStatement.parseStatement ["(","begin","skip","skip",")"])
									print (Parser.preSplit (reConcat (tail x)))
									print (Parser.myParse (reConcat (tail x)))
									let newState = Parser.myParse ( reConcat (tail x) ) ; (newVariable,output) = Tree.runNode (newState,variable,"") in printValueandWork newState newVariable output
								else 
									if ((head x) == ":t")
										then do
											print state
											replWork state variable
										else do
											print "Invalid operation"
											replWork state variable
	
	normalWork input operator output = do
		--print (Ratio.approxRational (read "0.1" :: Double) 0.01)
		--print (Variable.parseVariable "a")
		--print (ParseExpr.parseExpr ["10"])
		--print (ParseStatement.parseMakeVector ["a","10"])
		--print (Parser.myParse input)
		if (operator == "value")
			then do
				let (state,result) = Tree.runNode (Parser.myParse input,Map.empty,"") in
					if (output == "")
						then print result
						else writeFile output result
			else 
				if (output == "")
					then print (Parser.myParse input)
					else writeFile output (show (Parser.myParse input))
	
	mainWork m =  do
		if (Map.member "-repl" m)
			then replWork Tree.Nil Map.empty 
			else
				if (Map.member "-i" m)
					then do
						input <- readFile (Map.findWithDefault "error" "-i" m)
						normalWork input "value" (Map.findWithDefault "" "-o" m)
					else 
						if (Map.member "-t" m)
							then do
								input <- readFile (Map.findWithDefault "error" "-t" m)
								normalWork input "ast" (Map.findWithDefault "" "-o" m)
							else print "Parameter not enough"

	main :: IO()
	main = do
--		line <- getLine
		a <- getArgs
		print a
		mainWork (analyzeArgs a)
--		print (valueOfExpr (getASTTree line))
