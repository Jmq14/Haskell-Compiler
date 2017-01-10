module Translator where
	import qualified Function as Function
	import qualified Expr as Expr
	import qualified Tree as Tree
	import qualified Variable as Variable
	import qualified Data.Map as Map
	import qualified Debug.Trace as Trace;

--  translate function (entrance)
	genFunctionList :: (Int, Map.Map (Variable.Variable,Integer) Function.Function) -> String
	genFunctionList (indent, funcList) = 
		let list = Map.toList funcList in concat [(replicate indent ' ') ++ "def " ++ show function ++ "(" ++ genParam varList ++ "):\n" ++ genNode ((indent+4), node) | (_, Function.NewFunction function numVar varList node) <- list]

	genParam :: [Variable.Variable] -> String
	genParam [] = ""
	genParam (x:xs) = show x ++ "," ++ genParam xs

--Generate code of Tree Nodes
	genNode :: (Int, Tree.Node) -> String

--StatementList	
	genNode (indent, Tree.StatementListNode stmt next) = genNode (indent, stmt) ++ "\n" ++ genNode (indent, next)

--Nil
	genNode (indent, Tree.Nil) = "\n"

--Print
	genNode (indent, Tree.PrintNode expr) = replicate indent ' ' ++ "print " ++ genExpr expr

--Return 
	genNode (indent, Tree.ReturnNode expr) = replicate indent ' ' ++ "return " ++ genExpr expr

--Generate code of Expressions
	genExpr :: Expr.Expr -> String
	genExpr (Expr.NewConstant c) = show c