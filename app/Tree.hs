module Tree where
	import qualified Expr as Expr;
	import qualified Variable as Variable;

	import qualified Data.Map as Map;
	
	data Node = Nil | StatementListNode Node Node | SetVariableNode Variable.Variable Expr.Expr | WhileNode Expr.Expr Node | IfNode Expr.Expr Node Node | ErrorNode deriving (Show, Eq);

	runInNode :: (Node,Map.Map Variable.Variable Expr.Constant) -> Map.Map Variable.Variable Expr.Constant
	runInNode (WhileNode condition statement,variable) =
		let newVariable = runNode (statement,variable) in runNode (WhileNode condition statement,newVariable)
	runInNode _ = error "sb"

	runNode :: (Node,Map.Map Variable.Variable Expr.Constant) -> Map.Map Variable.Variable Expr.Constant
	runNode (Nil,variable) = variable;
	runNode (StatementListNode n1 n2,variable) = let newVariable = runNode (n1,variable) in runNode (n2,newVariable)
	runNode (SetVariableNode nowVariable expr,variable) = Map.insert nowVariable (Expr.valueOfExpr expr) variable
	runNode (WhileNode condition statement,variable) =
		let value = Expr.valueOfExpr condition in
			if (value == Expr.BoolConstant True)
				then runInNode (WhileNode condition statement,variable)
				else
					if (value == Expr.BoolConstant False)
						then runInNode (WhileNode condition statement,variable)
						else error "sb"
	runNode (IfNode condition branch1 branch2,variable) = 
		let value = Expr.valueOfExpr condition in
			if (value == Expr.BoolConstant True)
				then runNode (branch1,variable)
				else
					if (value == Expr.BoolConstant False)
						then runNode (branch2,variable)
						else error "sb"
