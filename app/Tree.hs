module Tree where
	import qualified Expr as Expr;
	import qualified Variable as Variable;

	import qualified Data.Map as Map;

	import qualified Debug.Trace as Trace;
	
	data Node = Nil | StatementListNode Node Node | SetVariableNode Variable.Variable Expr.Expr | WhileNode Expr.Expr Node | IfNode Expr.Expr Node Node | PrintNode Expr.Expr | MakeVectorNode Variable.Variable Expr.Expr | VectorSetNode Variable.Variable Expr.Expr Expr.Expr | ErrorNode deriving (Show, Eq);

	runInNode :: (Node,Map.Map Variable.Variable Expr.Constant,String) -> (Map.Map Variable.Variable Expr.Constant,String)
	runInNode (WhileNode condition statement,variable,output) =
		let (newVariable,newOutput) = runNode (statement,variable,output) in runNode (WhileNode condition statement,newVariable,newOutput)
	runInNode _ = error "sb"

	runNode :: (Node,Map.Map Variable.Variable Expr.Constant,String) -> (Map.Map Variable.Variable Expr.Constant,String)
	runNode (ErrorNode,variable,output) = (variable,"error")
	runNode (Nil,variable,output) = (variable,output);
	runNode (StatementListNode n1 n2,variable,output) = let (newVariable,newOutput) = runNode (n1,variable,output) in runNode (n2,newVariable,newOutput)
	runNode (SetVariableNode nowVariable expr,variable,output) = (Map.insert nowVariable (Expr.valueOfExpr expr variable) variable,output)
	runNode (WhileNode condition statement,variable,output) =
		let value = Expr.valueOfExpr condition variable in
			if (value == (Expr.BoolConstant True))
				then runInNode (WhileNode condition statement,variable,output)
				else
					if (value == (Expr.BoolConstant False))
						then (variable,output)
						else error "sb"
	runNode (IfNode condition branch1 branch2,variable,output) = 
		let value = Expr.valueOfExpr condition variable in
			if (value == (Expr.BoolConstant True))
				then runNode (branch1,variable,output)
				else
					if (value == (Expr.BoolConstant False))
						then runNode (branch2,variable,output)
						else error "sb"
	runNode (PrintNode expr,variable,output) =
		let var = Expr.valueOfExpr expr variable in (variable,output ++ (show var) ++ "\n")
	runNode (MakeVectorNode var len,variable,output) = 
		let lenValue = Expr.valueOfExpr len variable in
			if ((Expr.checkConstantWhetherInt lenValue))
				then let lenCons = Expr.convertConstantToInteger lenValue in 
					if (lenCons>0)
						then (Map.insert var (Expr.ArrayConstant lenCons Map.empty) variable,output)
						else error "sb"
				else error "sb"
	runNode (VectorSetNode var idx value,variable,output) = 
		let varValue = Map.findWithDefault Expr.ErrorConstant var variable ; idxValue = Expr.valueOfExpr idx variable ; cons = Expr.valueOfExpr value variable in
			if ((Expr.checkConstantWhetherArray varValue) && (Expr.checkConstantWhetherInt idxValue))
				then (Map.insert var (Expr.updateArrayValue varValue (Expr.convertConstantToInteger idxValue) cons) variable,output)
				else error "sb"
