module Run where
	import qualified Function as Function
	import qualified Expr as Expr
	import qualified Tree as Tree
	import qualified Variable as Variable

	import qualified Data.Map as Map
	
	import qualified Debug.Trace as Trace;

--	Tools
	visitVariable :: Variable.Variable -> (Map.Map Variable.Variable Expr.Constant) -> (Map.Map Variable.Variable Expr.Constant) -> Expr.Constant
	visitVariable var globalVariable localVariable =
		let value = Map.findWithDefault Expr.ErrorConstant var localVariable in
			if (value == Expr.ErrorConstant)
				then let res = Map.findWithDefault Expr.ErrorConstant var globalVariable in
					if (res == Expr.ErrorConstant)
						then error "sb"
						else res
				else value

	updateVariable :: Variable.Variable -> Expr.Constant -> Map.Map Variable.Variable Expr.Constant -> Map.Map Variable.Variable Expr.Constant -> Expr.Constant -> (Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Expr.Constant)
	updateVariable nowVariable value globalVariable localVariable returnValue =
		if ((Map.findWithDefault Expr.ErrorConstant nowVariable localVariable) == Expr.ErrorConstant)
			then (Map.insert nowVariable value globalVariable,localVariable,returnValue)
			else (globalVariable,Map.insert nowVariable value localVariable,returnValue)

	lookupFunction :: Variable.Variable -> Integer -> (Map.Map (Variable.Variable,Integer) Function.Function) -> Function.Function
	lookupFunction functionName numParm functionList =
		let result = Map.findWithDefault Function.ErrorFunction (functionName,numParm) functionList in
			if (result == Function.ErrorFunction)
				then error "sb"
				else result

	createLocalVariable :: [Variable.Variable] -> [Expr.Constant] -> (Map.Map Variable.Variable Expr.Constant)
	createLocalVariable [] [] = Map.empty
	createLocalVariable (x:xs) (y:ys) = Map.insert x y (createLocalVariable xs ys)
	createLocalVariable _ _ = error "sb"

--	Run function mod

	runFunction :: (Variable.Variable,Integer,[Expr.Constant],Map.Map (Variable.Variable,Integer) Function.Function,Map.Map Variable.Variable Expr.Constant) -> (Map.Map Variable.Variable Expr.Constant,Expr.Constant)
	runFunction (functionName,numParm,params,functionList,globalVariable) =
		let (Function.NewFunction var num varlist node) = lookupFunction functionName numParm functionList ;
			(newGlobalVariable,_,returnValue) = runNode (node,globalVariable,createLocalVariable varlist params,functionList,Expr.ErrorConstant)
		in (newGlobalVariable,returnValue)


--	Run statement mod
	runInNode :: (Tree.Node,Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Map.Map (Variable.Variable,Integer) Function.Function,Expr.Constant) -> (Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Expr.Constant)
	runInNode (Tree.WhileNode condition statement,globalVariable,localVariable,functionList,returnValue) =
		if (returnValue == Expr.ErrorConstant)
			then let (newGlobalVariable,newLocalVariable,newReturnValue) = runNode (statement,globalVariable,localVariable,functionList,returnValue) in runNode (Tree.WhileNode condition statement,newGlobalVariable,newLocalVariable,functionList,newReturnValue)
			else (globalVariable,localVariable,returnValue)
	runInNode _ = error "sb"

	runNode :: (Tree.Node,Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Map.Map (Variable.Variable,Integer) Function.Function,Expr.Constant) -> (Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Expr.Constant)

	runNode (Tree.ErrorNode,globalVariable,localVariable,functionList,returnValue) = error "sb"

	runNode (Tree.Nil,globalVariable,localVariable,functionList,returnValue) = (globalVariable,localVariable,returnValue)

	runNode (Tree.StatementListNode n1 n2,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then let (newGlobalVariable,newLocalVariable,newReturnValue) = runNode (n1,globalVariable,localVariable,functionList,returnValue) in runNode (n2,newGlobalVariable,newLocalVariable,functionList,newReturnValue)
			else (globalVariable,localVariable,returnValue)

	runNode (Tree.SetVariableNode nowVariable expr,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then let (value,newGlobalVariable) = valueOfExpr expr globalVariable localVariable functionList in updateVariable nowVariable value newGlobalVariable localVariable returnValue
			else (globalVariable,localVariable,returnValue)

	runNode (Tree.WhileNode condition statement,globalVariable,localVariable,functionList,returnValue) =
		if (returnValue == Expr.ErrorConstant)
			then
				let (value,newGlobalVariable) = valueOfExpr condition globalVariable localVariable functionList in
					if (value == (Expr.BoolConstant True))
						then runInNode (Tree.WhileNode condition statement,newGlobalVariable,localVariable,functionList,returnValue)
						else
							if (value == (Expr.BoolConstant False))
								then (globalVariable,newGlobalVariable,returnValue)
								else error "sb"
			else (globalVariable,localVariable,returnValue)


	runNode (Tree.IfNode condition branch1 branch2,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then
				let (value,newGlobalVariable) = valueOfExpr condition globalVariable localVariable functionList in
					if (value == (Expr.BoolConstant True))
						then runNode (branch1,newGlobalVariable,localVariable,functionList,returnValue)
						else
							if (value == (Expr.BoolConstant False))
								then runNode (branch2,newGlobalVariable,localVariable,functionList,returnValue)
								else error "sb"
			else (globalVariable,localVariable,returnValue)

	runNode (Tree.PrintNode expr,globalVariable,localVariable,functionList,returnValue) =
		if (returnValue == Expr.ErrorConstant)
			then let (var,newGlobalVariable) = valueOfExpr expr globalVariable localVariable functionList in Trace.trace (show var) (newGlobalVariable,localVariable,returnValue)
			else (globalVariable,localVariable,returnValue)

	runNode (Tree.MakeVectorNode var len,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then
				let (lenValue,newGlobalVariable) = valueOfExpr len globalVariable localVariable functionList in
					if ((Expr.checkConstantWhetherInt lenValue))
						then let lenCons = Expr.convertConstantToInteger lenValue in 
							if (lenCons>0)
								then updateVariable var (Expr.ArrayConstant lenCons Map.empty) newGlobalVariable localVariable returnValue
								else error "sb"
						else error "sb"
			else (globalVariable,localVariable,returnValue)

	runNode (Tree.VectorSetNode var idx value,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then
				let varValue = visitVariable var globalVariable localVariable; 
					(idxValue,newGlobalVariable1) = valueOfExpr idx localVariable globalVariable functionList ; 
					(cons,newGlobalVariable2) = valueOfExpr value localVariable newGlobalVariable1 functionList in
					if ((Expr.checkConstantWhetherArray varValue) && (Expr.checkConstantWhetherInt idxValue))
						then updateVariable var (Expr.updateArrayValue varValue (Expr.convertConstantToInteger idxValue) cons) newGlobalVariable2 localVariable returnValue
						else error "sb"
			else (globalVariable,localVariable,returnValue)

	runNode (Tree.ReturnNode expr,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then let (value,newGlobalVariable) = valueOfExpr expr globalVariable localVariable functionList in (newGlobalVariable,localVariable,value)
			else (globalVariable,localVariable,returnValue)

--	Run expr mod
	valueOfExpr :: Expr.Expr -> Map.Map Variable.Variable Expr.Constant -> Map.Map Variable.Variable Expr.Constant -> Map.Map (Variable.Variable,Integer) Function.Function -> (Expr.Constant,Map.Map Variable.Variable Expr.Constant)
	valueOfExpr Expr.EmptyExpr _ _ _= error "Something Wrong!";
	valueOfExpr (Expr.NewConstant (Expr.BoolConstant x)) globalVariable localVariable functionList  = (Expr.BoolConstant x,globalVariable);
	valueOfExpr (Expr.NewConstant (Expr.FloatConstant x)) globalVariable localVariable functionList = (Expr.FloatConstant x,globalVariable);
	valueOfExpr (Expr.NewConstant (Expr.CharConstant x)) globalVariable localVariable functionList = (Expr.CharConstant x,globalVariable);
	valueOfExpr (Expr.NewConstant (Expr.StringConstant x)) globalVariable localVariable functionList = (Expr.StringConstant x,globalVariable);
	valueOfExpr (Expr.NewConstant Expr.ErrorConstant) _ _ _ = error "sb";

	valueOfExpr (Expr.NewConstant (Expr.VariableConstant nowvar)) globalVariable localVariable functionList = (visitVariable nowvar globalVariable localVariable,globalVariable)

	valueOfExpr (Expr.NewExpr operator datatype expr1 expr2) globalVariable localVariable functionList
		| operator == Expr.NotOperator	= let (value,newGlobalVariable) = (valueOfExpr expr1 globalVariable localVariable functionList) in (Expr.notConstant value,newGlobalVariable)
		| operator == Expr.OrOperator	= let (leftValue,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList in
			if (Expr.checkWhetherBool leftValue == True)
				then
					if (leftValue == (Expr.BoolConstant True))
						then (leftValue,newGlobalVariable1)
						else let (rightValue,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
							if (Expr.checkWhetherBool rightValue == True)
								then (rightValue,newGlobalVariable2)
								else error "sb"
				else error "sb"
		| operator == Expr.AndOperator	= let (leftValue,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList in
			if (Expr.checkWhetherBool leftValue == True)
				then
					if (leftValue == (Expr.BoolConstant False))
						then (leftValue,newGlobalVariable1)
						else let (rightValue,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
							if (Expr.checkWhetherBool rightValue == True)
								then (rightValue,newGlobalVariable2)
								else error "sb"
				else error "sb"
		| operator == Expr.PlusOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.plusConstant value1 value2,newGlobalVariable2)
		| operator == Expr.MinusOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.minusConstant value1 value2,newGlobalVariable2)
		| operator == Expr.MultiplicationOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.multiplicationConstant value1 value2,newGlobalVariable2)
		| operator == Expr.DivisionOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.divisionConstant value1 value2,newGlobalVariable2)
		| operator == Expr.EqualOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.equalConstant value1 value2,newGlobalVariable2)
		| operator == Expr.LessOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.lessConstant value1 value2,newGlobalVariable2)
		| operator == Expr.LeqOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.leqConstant value1 value2,newGlobalVariable2)
		| operator == Expr.GreatOperator = 
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.greatConstant value1 value2,newGlobalVariable2)
		| operator == Expr.GeqOperator = 
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.geqConstant value1 value2,newGlobalVariable2)
		| operator == Expr.ConsOperator = 
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.consConstant value1 value2,newGlobalVariable2)
		| operator == Expr.CarOperator = let (value,newGlobalVariable) = valueOfExpr expr1 globalVariable localVariable functionList in (Expr.carConstant value,newGlobalVariable)
		| operator == Expr.CdrOperator = let (value,newGlobalVariable) = valueOfExpr expr1 globalVariable localVariable functionList in (Expr.cdrConstant value,newGlobalVariable)
	
	valueOfExpr (Expr.ArrayExpr var expr) globalVariable localVariable functionList =
		let (value,newGlobalVariable) = valueOfExpr expr globalVariable localVariable functionList; nowvar = visitVariable var newGlobalVariable localVariable in 
			if ((Expr.checkConstantWhetherInt value) && (Expr.checkConstantWhetherArray nowvar))
				then (Expr.visitArrayValue nowvar (Expr.convertConstantToInteger value),newGlobalVariable)
				else error "sb"
					
	valueOfExpr (Expr.FunctionExpr functionName numParm params) globalVariable localVariable functionList =	
		let (paramsValue,newGlobalVariable1) = getValueOfParams params globalVariable localVariable functionList ;
			(newGlobalVariable2,value) = runFunction (functionName,numParm,paramsValue,functionList,newGlobalVariable1) in
			(value,newGlobalVariable2)

	getValueOfParams :: [Expr.Expr] -> Map.Map Variable.Variable Expr.Constant -> Map.Map Variable.Variable Expr.Constant -> Map.Map (Variable.Variable,Integer) Function.Function -> ([Expr.Constant],Map.Map Variable.Variable Expr.Constant)
	getValueOfParams [] globalVariable localVariable functionList = ([],globalVariable)
	getValueOfParams (x:xs) globalVariable localVariable functionList = 
		let (value,newGlobalVariable1) = valueOfExpr x globalVariable localVariable functionList ;
			(valueList,newGlobalVariable2) = getValueOfParams xs newGlobalVariable1 localVariable functionList in
			(value:valueList,newGlobalVariable2)
