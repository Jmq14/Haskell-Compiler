module Main where
	import qualified Parser as Parser
	import qualified Expr as Expr
	import qualified Tree as Tree
	import qualified Variable as Variable

	import qualified ParseStatement as ParseStatement
	import qualified ParseExpr as ParseExpr
	import qualified ParseFunction as ParseFunction

	import qualified Run as Run

	import qualified Repl as Repl

	import System.Environment
	import System.Exit
	import System.IO

	import GHC.IO.Handle

	import Control.DeepSeq

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
	
	normalWork input operator output = do
		let functionList = Parser.myParse input in do
			if (output /= "")
				then do
					b <- writeFile output ""
					fileHandle <- openFile output WriteMode
					c <- hDuplicateTo fileHandle stderr
					d <- hDuplicateTo fileHandle stdout
					putStr ""
				else do
					putStr ""
			if (operator == "value")
				then do
					--(Run.runFunction (Variable.NewVariable "main",0,[],functionList,Map.empty)) `deepseq` putStr ""
					let (globalVariable,returnValue) = Run.runFunction (Variable.NewVariable "main",0,[],functionList,Map.empty) in putStrLn ("return value:" ++ (show returnValue))
				else do
					putStr ""--functionList
			putStr ""

	mainWork m =  do
		if (Map.member "-repl" m)
			then Repl.mainWork
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
							else putStrLn "Parameter not enough"

	main :: IO()
	main = do
--		line <- getLine
		a <- getArgs
		print a
		mainWork (analyzeArgs a)
--		print (valueOfExpr (getASTTree line))
