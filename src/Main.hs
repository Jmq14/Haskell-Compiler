module Main where
	import Parser
	import Expr
	import System.Environment

	getASTTree s = Parser.parse s;

	main :: IO()
	main = do
		line <- getLine
		print (valueOfExpr (getASTTree line))
