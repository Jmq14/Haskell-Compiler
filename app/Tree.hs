module Tree where
	import qualified Expr as Expr;
	import qualified Variable as Variable;

	import qualified Data.Map as Map;
	
	data Node = Nil | StatementListNode Node Node | SetVariableNode Variable.Variable Expr.Expr | WhileNode Expr.Expr Node | IfNode Expr.Expr Node Node | PrintNode Expr.Expr | ErrorNode deriving (Show, Eq);

	runInNode :: (Node,Map.Map Variable.Variable Expr.Constant,String) -> Map.Map Variable.Variable Expr.Constant
	runInNode (WhileNode condition statement,variable,filename) =
		let newVariable = runNode (statement,variable,filename) in runNode (WhileNode condition statement,newVariable,filename)
	runInNode _ = error "sb"

	runNode :: (Node,Map.Map Variable.Variable Expr.Constant,String) -> Map.Map Variable.Variable Expr.Constant
	runNode (Nil,variable,_) = variable;
	runNode (StatementListNode n1 n2,variable,filename) = let newVariable = runNode (n1,variable,filename) in runNode (n2,newVariable,filename)
	runNode (SetVariableNode nowVariable expr,variable,fileanme) = Map.insert nowVariable (Expr.valueOfExpr expr variable) variable
	runNode (WhileNode condition statement,variable,filename) =
		let value = Expr.valueOfExpr condition variable in
			if (value == (Expr.BoolConstant True))
				then runInNode (WhileNode condition statement,variable,filename)
				else
					if (value == (Expr.BoolConstant False))
						then runInNode (WhileNode condition statement,variable,filename)
						else error "sb"
	runNode (IfNode condition branch1 branch2,variable,filename) = 
		let value = Expr.valueOfExpr condition variable in
			if (value == (Expr.BoolConstant True))
				then runNode (branch1,variable,filename)
				else
					if (value == (Expr.BoolConstant False))
						then runNode (branch2,variable,filename)
						else error "sb"
	runNode (PrintNode expr,variable,filename) =
		let x = 
			if (filename == "")
				then print (Expr.valueOfExpr expr variable) 
				else writeFile filename (show (Expr.valueOfExpr expr variable))
		in variable
