module Translator where
	import qualified Function as Function
	import qualified Expr as Expr
	import qualified Tree as Tree
	import qualified Variable as Variable
	import qualified Data.Map as Map
	import qualified Debug.Trace as Trace;

--  translate function (entrance)
	translate2python :: Map.Map (Variable.Variable,Integer) Function.Function -> String
	translate2python funcList = 
		let globalVariables = getGlobal funcList in
		genFunctionList (0, funcList, globalVariables)

	genFunctionList :: (Int, Map.Map (Variable.Variable,Integer) Function.Function, [Variable.Variable]) -> String
	genFunctionList (indent, funcList, globalVariables) = 
		let list = Map.toList funcList in 
		(concat [(replicate indent ' ') ++ "def " ++ show function ++ "(" ++ genParam varList ++ "):\n" ++ refGlobal (indent+4) globalVariables varList ++ genNode ((indent+4), node) ++ "\n" | (_, Function.NewFunction function numVar varList node) <- list]) ++ declGlobal globalVariables ++ "\nmain()"

	genParam :: [Variable.Variable] -> String
	genParam [] = ""
	genParam (x:[]) = show x 
	genParam (x:xs) = show x ++ "," ++ genParam xs

	genArguments :: [Expr.Expr] -> String
	genArguments [] = ""
	genArguments (x:[]) = genExpr x 
	genArguments (x:xs) = genExpr x ++ ", " ++ genArguments xs

	getGlobal :: Map.Map (Variable.Variable,Integer) Function.Function -> [Variable.Variable]
	getGlobal funcList = 
		let list = Map.toList funcList in 
		concat [findSetStmt node []| (_, Function.NewFunction function numVar varList node) <- list]

	declGlobal :: [Variable.Variable] -> String
	declGlobal globalVariables = 
		concat [show v ++ " = None\n"|v <- globalVariables]

	refGlobal :: Int -> [Variable.Variable] -> [Variable.Variable] -> String
	refGlobal indent globalVariables varList = 
		concat [if v `elem` varList then "" else (replicate indent ' ' ++ "global " ++ show v ++ "\n") | v <- globalVariables]

	findSetStmt :: Tree.Node -> [Variable.Variable] -> [Variable.Variable]
	findSetStmt (Tree.SetVariableNode v _) globalVariables
		|show v == "void" = []
		|v `elem` globalVariables  = []
		|otherwise = [v]
	findSetStmt (Tree.MakeVectorNode v _) globalVariables
		|show v == "void" = []
		|v `elem` globalVariables  = []
		|otherwise = [v]
	findSetStmt (Tree.StatementListNode stmt next) globalVariables = 
		let v = findSetStmt stmt globalVariables in
		v ++ findSetStmt next (globalVariables++v)
	findSetStmt _ globalVariables = []

-------------
--Generate code of Tree Nodes
	genNode :: (Int, Tree.Node) -> String

--Statement List	
	genNode (indent, Tree.StatementListNode stmt next) = genNode (indent, stmt) ++ "\n" ++ genNode (indent, next)

--Nil
	genNode (indent, Tree.Nil) = ""

--Error
	genNode (indent, Tree.ErrorNode) = error "syntax error occurs!"

--Set Variable
	genNode (indent, Tree.SetVariableNode (Variable.NewVariable "void") expr) = 
		replicate indent ' ' ++ genExpr expr
	genNode (indent, Tree.SetVariableNode nowVariable expr) = 
		replicate indent ' ' ++ show nowVariable ++ " = " ++ genExpr expr

--While
	genNode (indent, Tree.WhileNode condition statement) = 
		replicate indent ' ' ++ "while " ++ genExpr condition ++ ":\n" ++ genNode (indent+4, statement)

--If
	genNode (indent, Tree.IfNode condition branch1 Tree.Nil) = 
		replicate indent ' ' ++ "if " ++ genExpr condition ++ ":\n" ++ genNode (indent+4, branch1) 

	genNode (indent, Tree.IfNode condition branch1 branch2) = 
		replicate indent ' ' ++ "if " ++ genExpr condition ++ ":\n" ++ genNode (indent+4, branch1) ++ replicate indent ' ' ++ "else:\n" ++ genNode (indent+4, branch2)

--Make Vector
	genNode (indent, Tree.MakeVectorNode var len) = 
		replicate indent ' ' ++ show var ++ " = [None] * (" ++ genExpr len ++ ")"

--Set Vectore
	genNode (indent, Tree.VectorSetNode var idx value) =
		replicate indent ' ' ++ show var ++ "[" ++ genExpr idx ++ "] = " ++ genExpr value  

--Print
	genNode (indent, Tree.PrintNode expr) = replicate indent ' ' ++ "print(" ++ genExpr expr ++ ")"

--Return 
	genNode (0, Tree.ReturnNode _) = ""
	genNode (indent, Tree.ReturnNode expr) = replicate indent ' ' ++ "return " ++ genExpr expr

-------------
--Generate code of Expressions
	genExpr :: Expr.Expr -> String

--Empty
	genExpr (Expr.EmptyExpr) = ""

--Constant
	genExpr (Expr.NewConstant c) = show c

--Expressions
	genExpr (Expr.NewExpr Expr.NotOperator _ expr1 expr2) = "(not " ++ genExpr expr1 ++ ")"
	genExpr (Expr.NewExpr Expr.AndOperator _ expr1 expr2) = "(" ++ genExpr expr1 ++ " and " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.OrOperator _  expr1 expr2) = "(" ++ genExpr expr1 ++ " or " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.PlusOperator _ expr1 expr2) = "(" ++ genExpr expr1 ++ " + " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.MinusOperator _ expr1 expr2) = "(" ++ genExpr expr1 ++ " - " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.MultiplicationOperator _ expr1 expr2) = genExpr expr1 ++ " * " ++ genExpr expr2
	genExpr (Expr.NewExpr Expr.DivisionOperator _ expr1 expr2) = genExpr expr1 ++ " / " ++ genExpr expr2
	genExpr (Expr.NewExpr Expr.EqualOperator _ expr1 expr2) = "(" ++ genExpr expr1 ++ " == " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.LessOperator _ expr1 expr2) = "(" ++ genExpr expr1 ++ " < " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.LeqOperator _ expr1 expr2) = "(" ++ genExpr expr1 ++ " <= " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.GreatOperator _ expr1 expr2) = "(" ++ genExpr expr1 ++ " > " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.GeqOperator _ expr1 expr2) = "(" ++ genExpr expr1 ++ " >= " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.ConsOperator _ expr1 expr2) = "(" ++ genExpr expr1 ++ ", " ++ genExpr expr2 ++ ")"
	genExpr (Expr.NewExpr Expr.CarOperator _ expr _) = "(" ++ genExpr expr ++ "[0])"
	genExpr (Expr.NewExpr Expr.CdrOperator _ expr _) = "(" ++ genExpr expr ++ "[1])"
--	genExpr (Expr.NewExpr Expr.CarOperator (Expr.PairType l r) _ _) = genExpr l 
--	genExpr (Expr.NewExpr Expr.CdrOperator (Expr.PairType l r) _ _) = genExpr r

--Function Call
	genExpr (Expr.FunctionExpr function numVar params) = show function ++ "(" ++ genArguments params ++ ")"

--Array Reference
	genExpr (Expr.ArrayExpr array indx) = show array ++ "[" ++ genExpr indx ++ "]"



