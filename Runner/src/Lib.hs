module Lib where
	import qualified Parser as Parser
	import qualified Expr as Expr
	import qualified Tree as Tree
	import qualified Variable as Variable

	import qualified ParseStatement as ParseStatement
	import qualified ParseExpr as ParseExpr
	import qualified ParseFunction as ParseFunction

	import qualified Run as Run
	import qualified PrettyPrinter as PrettyPrinter

	import qualified Repl as Repl

	import System.Environment
	import System.Exit
	import System.IO

	import GHC.IO.Handle

	import Control.DeepSeq
	import qualified Control.Exception as Exception

	import qualified Data.Map as Map
	import qualified Data.List.Split as Split
	import qualified Data.Ratio as Ratio

	-- 分析调用参数
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

	-- 捕捉异常信息
	catchAny :: IO a -> (Exception.SomeException -> IO a) -> IO a
	catchAny = Exception.catch

	normalMindWork operator functionList = do
		if (operator == "value")
			then do
				let (globalVariable,returnValue) = Run.runFunction (Variable.NewVariable "main",0,[],functionList,Map.empty) in putStrLn ("return value:" ++ (Expr.notPrettyShow returnValue))
			else do
				PrettyPrinter.prettyPrinter $ map (\(x,y)->y) $ Map.toAscList functionList

	-- 非repl的工作函数
	normalMind input operator output = do
		let functionList = Parser.myParse input in do
			if (output /= "")
				then do
					b <- writeFile output ""
					fileHandle <- openFile output WriteMode
					c <- hDuplicateTo fileHandle stderr
					normalMindWork operator functionList
					q <- hClose fileHandle
					-- d <- hDuplicateTo fileHandle stdout
					putStr ""
				else do
					normalMindWork operator functionList
					putStr ""
			putStr ""
	
	-- 捕捉非repl模式的工作异常并输出
	normalWork input operator output = do
		catchAny (normalMind input operator output) (\err -> do 
			putStrLn (head (Split.splitOn "\n" (show err)))
			putStr "")

	-- 根据系统参数选择工作模式
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

	-- 主函数
	runner :: IO()
	runner = do
		a <- getArgs
		mainWork (analyzeArgs a)
