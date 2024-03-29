module Repl where
	import qualified Tree as Tree
	import qualified Expr as Expr
	import qualified Function as Function
	import qualified Variable as Variable

	import qualified ParseExpr as ParseExpr
	import qualified ParseStatement as ParseStatement
	import qualified ParseFunction as ParseFunction
	import qualified Parser as Parser
	import qualified PrettyPrinter as PrettyPrinter

	import qualified Run as Run

	import qualified KeyWord as KeyWord

	import qualified Data.Map as Map
	import qualified Data.List.Split as Split

	import qualified Control.Exception as Exception

	-- Repl专属数据类型，用于Repl中所有可能出现的情况
	data FunctionOrTreeOrExpr = Nil | NewFunction Variable.Variable Integer Function.Function | NewNode Tree.Node | NewExpr Expr.Expr deriving (Show, Eq);
	
	-- 将分割的字符串进行重新连接
	reConcat :: [String] -> String
	reConcat [] = ""
	reConcat (x:xs) = x ++ " " ++ (reConcat xs)

	-- Repl模式的解析表达式
	replParseExpr :: [String] -> FunctionOrTreeOrExpr
	replParseExpr x =
		let (expr,ahead) = ParseExpr.parseExpr x in
			if (ahead == [])
				then NewExpr expr
				else error "Compile Error: there is something more in the last"
	
	-- Repl模式的解析语句
	replParseStatement :: [String] -> FunctionOrTreeOrExpr
	replParseStatement x =
		let (statement,ahead) = ParseStatement.parseStatement x in
			if (ahead == [])
				then NewNode statement
				else error "Compile Error: there is something more in the last"

	-- Repl模式的解析函数
	replParseFunction :: [String] -> FunctionOrTreeOrExpr
	replParseFunction x =
		let (functionName,numVar,function,ahead) = ParseFunction.parseFunction x in
			if (ahead == [])
				then NewFunction functionName numVar function
				else error "Compile Error: there is something more in the last"

	-- Repl模式解析
	replParse :: [String] -> FunctionOrTreeOrExpr
	replParse [] = error "Compile Error: is there anything?"
	replParse (x:xs) =
		if (x == "(")
			then
				if (xs == [])
					then error "Compile error: there is a missing \")\""
					else
						if ((head xs) == "define")
							then replParseFunction (x:xs)
							else
								if (KeyWord.insideOrNot (head xs) KeyWord.statementKeywords)
									then replParseStatement (x:xs)
									else replParseExpr (x:xs)								
			else
				if (x == "skip")
					then replParseStatement (x:xs)
					else replParseExpr (x:xs)

	-- Repl模式运行函数
	replRun (NewFunction functionName numVar function) variable functionList = do
		replWork (NewFunction functionName numVar function) variable (Map.insert (functionName,numVar) function functionList) 
	
	-- Repl模式运行语句
	replRun (NewNode node) variable functionList = 
		let (newVariable,_,returnValue) = Run.runNode (node,variable,Map.empty,functionList,Expr.ErrorConstant) in
			if (returnValue == Expr.ErrorConstant)
				then do
					putStr ""
					replWork (NewNode node) newVariable functionList 
				else do
					putStrLn ("Return " ++ (Expr.notPrettyShow returnValue))
					replWork (NewNode node) newVariable functionList 

	-- Repl模式计算表达式
	replRun (NewExpr expr) variable functionList =
		let (value,newVariable) = Run.valueOfExpr expr variable Map.empty functionList in do
			putStrLn (Expr.notPrettyShow value)
			replWork (NewExpr expr) newVariable functionList
	
	-- Repl模式的工作函数
	replMind x preWork variable functionList = do
		if ((head x) == ":q")
			then putStrLn "Goodbye World"
			else
				if ((head x) == ":i")
					then let parseResult = replParse (Parser.preSplit (reConcat (tail x))) in do
						replRun parseResult variable functionList
					else
						if ((head x) == ":t")
							then do
								PrettyPrinter.prettyPrinter [preWork]
								replWork preWork variable functionList
							else do
								putStrLn "Invalid operation"
								replWork preWork variable functionList

	-- 捕获异常
	catchAny :: IO a -> (Exception.SomeException -> IO a) -> IO a
	catchAny = Exception.catch

	-- Repl模式的外层函数，负责处理异常捕获
	replWork preWork variable functionList = do
		line <- getLine
		let x = Split.splitOn " " line in
			if (x == [""])
				then replWork preWork variable functionList
				else catchAny  (replMind x preWork variable functionList) (\err -> do
						putStrLn (head (Split.splitOn "\n" (show err)))
						replWork preWork variable functionList
				)
	
	-- Repl模式主函数
	mainWork = do
		replWork Nil Map.empty Map.empty 
