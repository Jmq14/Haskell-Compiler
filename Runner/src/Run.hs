module Run where
	import qualified Function as Function
	import qualified Expr as Expr
	import qualified Tree as Tree
	import qualified Variable as Variable

	import qualified Data.Map as Map
	
	import qualified Debug.Trace as Trace;

	-- 工具类函数
	-- 根据局部变量表和全局变量表访问变量
	visitVariable :: Variable.Variable -> (Map.Map Variable.Variable Expr.Constant) -> (Map.Map Variable.Variable Expr.Constant) -> Expr.Constant
	visitVariable var globalVariable localVariable =
		let value = Map.findWithDefault Expr.ErrorConstant var localVariable in
			if (value == Expr.ErrorConstant)
				then let res = Map.findWithDefault Expr.ErrorConstant var globalVariable in
					if (res == Expr.ErrorConstant)
						then error ("Runtime error: variable " ++ (show var) ++ " undefined")
						else res
				else value
	
	-- 更新变量值
	updateVariable :: Variable.Variable -> Expr.Constant -> Map.Map Variable.Variable Expr.Constant -> Map.Map Variable.Variable Expr.Constant -> Expr.Constant -> (Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Expr.Constant)
	updateVariable nowVariable value globalVariable localVariable returnValue =
		if ((Map.findWithDefault Expr.ErrorConstant nowVariable localVariable) == Expr.ErrorConstant)
			then (Map.insert nowVariable value globalVariable,localVariable,returnValue)
			else (globalVariable,Map.insert nowVariable value localVariable,returnValue)

	-- 在全局函数表搜索函数
	lookupFunction :: Variable.Variable -> Integer -> (Map.Map (Variable.Variable,Integer) Function.Function) -> Function.Function
	lookupFunction functionName numParm functionList =
		let result = Map.findWithDefault Function.ErrorFunction (functionName,numParm) functionList in
			if (result == Function.ErrorFunction)
				then error ("Runtime error: function " ++ (show functionName) ++ " with " ++ (show numParm) ++ " parameters is undefined")
				else result

	-- 创建局部变量表
	createLocalVariable :: [Variable.Variable] -> [Expr.Constant] -> (Map.Map Variable.Variable Expr.Constant)
	createLocalVariable [] [] = Map.empty
	createLocalVariable (x:xs) (y:ys) = Map.insert x y (createLocalVariable xs ys)
	createLocalVariable _ _ = error "This can't be reached! If you reach here, uhm, this not the bug, this is the feature! Congratulation that you get acheivement 1!(createLocalVariable)"
	
	-- 运行函数
	runFunction :: (Variable.Variable,Integer,[Expr.Constant],Map.Map (Variable.Variable,Integer) Function.Function,Map.Map Variable.Variable Expr.Constant) -> (Map.Map Variable.Variable Expr.Constant,Expr.Constant)
	runFunction (functionName,numParm,params,functionList,globalVariable) =
		let (Function.NewFunction var num varlist node) = lookupFunction functionName numParm functionList ;
			(newGlobalVariable,_,returnValue) = runNode (node,globalVariable,createLocalVariable varlist params,functionList,Expr.ErrorConstant)
		in 
			if (returnValue == Expr.ErrorConstant)
				then error "Runtime error: function should have return value"
				else (newGlobalVariable,returnValue)

	-- 运行语句
	-- 运行while语句内部语句
	runInNode :: (Tree.Node,Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Map.Map (Variable.Variable,Integer) Function.Function,Expr.Constant) -> (Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Expr.Constant)
	runInNode (Tree.WhileNode condition statement,globalVariable,localVariable,functionList,returnValue) =
		if (returnValue == Expr.ErrorConstant)
			then let (newGlobalVariable,newLocalVariable,newReturnValue) = runNode (statement,globalVariable,localVariable,functionList,returnValue) in runNode (Tree.WhileNode condition statement,newGlobalVariable,newLocalVariable,functionList,newReturnValue)
			else (globalVariable,localVariable,returnValue)
	runInNode _ = error "This can't be reached! If you reach here, uhm, this not the bug, this is the feature! Congratulation that you get acheivement 2!(runInNode)"

	-- 运行各种语句
	runNode :: (Tree.Node,Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Map.Map (Variable.Variable,Integer) Function.Function,Expr.Constant) -> (Map.Map Variable.Variable Expr.Constant,Map.Map Variable.Variable Expr.Constant,Expr.Constant)

	runNode (Tree.ErrorNode,globalVariable,localVariable,functionList,returnValue) = error "This can't be reached! If you reach here, uhm, this not the bug, this is the feature! Congratulation that you get acheivement 3!(runNode ErrorNode)"

	-- 运行skip语句
	runNode (Tree.Nil,globalVariable,localVariable,functionList,returnValue) = (globalVariable,localVariable,returnValue)

	-- 运行多条语句
	runNode (Tree.StatementListNode n1 n2,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then let (newGlobalVariable,newLocalVariable,newReturnValue) = runNode (n1,globalVariable,localVariable,functionList,returnValue) in runNode (n2,newGlobalVariable,newLocalVariable,functionList,newReturnValue)
			else (globalVariable,localVariable,returnValue)

	-- 运行变量赋值语句
	runNode (Tree.SetVariableNode nowVariable expr,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then let (value,newGlobalVariable) = valueOfExpr expr globalVariable localVariable functionList in updateVariable nowVariable value newGlobalVariable localVariable returnValue
			else (globalVariable,localVariable,returnValue)

	-- 运行while语句
	runNode (Tree.WhileNode condition statement,globalVariable,localVariable,functionList,returnValue) =
		if (returnValue == Expr.ErrorConstant)
			then
				let (value,newGlobalVariable) = valueOfExpr condition globalVariable localVariable functionList in
					if (value == (Expr.BoolConstant True))
						then runInNode (Tree.WhileNode condition statement,newGlobalVariable,localVariable,functionList,returnValue)
						else
							if (value == (Expr.BoolConstant False))
								then (newGlobalVariable,localVariable,returnValue)
								else error ("Runtime Error: " ++ (Expr.notPrettyShow value) ++ " is not an available while condition value")
			else (globalVariable,localVariable,returnValue)

	-- 运行if语句
	runNode (Tree.IfNode condition branch1 branch2,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then
				let (value,newGlobalVariable) = valueOfExpr condition globalVariable localVariable functionList in
					if (value == (Expr.BoolConstant True))
						then runNode (branch1,newGlobalVariable,localVariable,functionList,returnValue)
						else
							if (value == (Expr.BoolConstant False))
								then runNode (branch2,newGlobalVariable,localVariable,functionList,returnValue)
								else error ("Runtime Error: " ++ (Expr.notPrettyShow value) ++ " is not an available if condtion value")
			else (globalVariable,localVariable,returnValue)

	-- 运行print语句
	runNode (Tree.PrintNode expr,globalVariable,localVariable,functionList,returnValue) =
		if (returnValue == Expr.ErrorConstant)
			then let (var,newGlobalVariable) = valueOfExpr expr globalVariable localVariable functionList in Trace.trace (Expr.notPrettyShow var) (newGlobalVariable,localVariable,returnValue)
			else (globalVariable,localVariable,returnValue)

	-- 运行创建数组语句
	runNode (Tree.MakeVectorNode var len,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then
				let (lenValue,newGlobalVariable) = valueOfExpr len globalVariable localVariable functionList in
					if ((Expr.checkConstantWhetherInt lenValue))
						then let lenCons = Expr.convertConstantToInteger lenValue in 
							if (lenCons>0)
								then updateVariable var (Expr.ArrayConstant lenCons Map.empty) newGlobalVariable localVariable returnValue
								else error ("Runtime Error: " ++ (Expr.notPrettyShow lenValue) ++ " should be positive to be a vector's length")
						else error ("Runtime Erroor: " ++ (Expr.notPrettyShow lenValue) ++ " is not an available vector length")
			else (globalVariable,localVariable,returnValue)

	-- 运行数组赋值语句
	runNode (Tree.VectorSetNode var idx value,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then
				let varValue = visitVariable var globalVariable localVariable; 
					(idxValue,newGlobalVariable1) = valueOfExpr idx globalVariable localVariable functionList ; 
					(cons,newGlobalVariable2) = valueOfExpr value newGlobalVariable1 localVariable functionList in
					if (Expr.checkConstantWhetherArray varValue)
						then if (Expr.checkConstantWhetherInt idxValue)
							then updateVariable var (Expr.updateArrayValue varValue (Expr.convertConstantToInteger idxValue) cons) newGlobalVariable2 localVariable returnValue
							else error ("Runtime Error: " ++ (Expr.notPrettyShow idxValue) ++ " is not an available vector index")
						else error ("Runtime Error: " ++ (Expr.notPrettyShow varValue) ++ " is not a vector")
			else (globalVariable,localVariable,returnValue)

	-- 运行返回语句
	runNode (Tree.ReturnNode expr,globalVariable,localVariable,functionList,returnValue) = 
		if (returnValue == Expr.ErrorConstant)
			then let (value,newGlobalVariable) = valueOfExpr expr globalVariable localVariable functionList in (newGlobalVariable,localVariable,value)
			else (globalVariable,localVariable,returnValue)

	-- 表达式求值
	valueOfExpr :: Expr.Expr -> Map.Map Variable.Variable Expr.Constant -> Map.Map Variable.Variable Expr.Constant -> Map.Map (Variable.Variable,Integer) Function.Function -> (Expr.Constant,Map.Map Variable.Variable Expr.Constant)
	valueOfExpr Expr.EmptyExpr _ _ _= error "This can't be reached! If you reach here, uhm, this not the bug, this is the feature! Congratulation that you get acheivement 4!(valueOfExpr EmptyExpr)"
	valueOfExpr (Expr.NewConstant (Expr.BoolConstant x)) globalVariable localVariable functionList  = (Expr.BoolConstant x,globalVariable);
	valueOfExpr (Expr.NewConstant (Expr.FloatConstant x)) globalVariable localVariable functionList = (Expr.FloatConstant x,globalVariable);
	valueOfExpr (Expr.NewConstant (Expr.CharConstant x)) globalVariable localVariable functionList = (Expr.CharConstant x,globalVariable);
	valueOfExpr (Expr.NewConstant (Expr.StringConstant x)) globalVariable localVariable functionList = (Expr.StringConstant x,globalVariable);
	valueOfExpr (Expr.NewConstant Expr.ErrorConstant) _ _ _ = error "This can't be reached! If you reach here, uhm, this not the bug, this is the feature! Congratulation that you get acheivement 5!(valueOfExpr ErrorConstant)";

	valueOfExpr (Expr.NewConstant (Expr.VariableConstant nowvar)) globalVariable localVariable functionList = (visitVariable nowvar globalVariable localVariable,globalVariable)

	valueOfExpr (Expr.NewExpr operator datatype expr1 expr2) globalVariable localVariable functionList
		-- not运算
		| operator == Expr.NotOperator	= let (value,newGlobalVariable) = (valueOfExpr expr1 globalVariable localVariable functionList) in (Expr.notConstant value,newGlobalVariable)
		-- or运算，加入短路机制
		| operator == Expr.OrOperator	= let (leftValue,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList in
			if (Expr.checkWhetherBool leftValue == True)
				then
					if (leftValue == (Expr.BoolConstant True))
						then (leftValue,newGlobalVariable1)
						else let (rightValue,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
							if (Expr.checkWhetherBool rightValue == True)
								then (rightValue,newGlobalVariable2)
								else error ("Runtime Error: " ++ (Expr.notPrettyShow rightValue) ++ " is not an available value in or operator")
				else error ("Runtime Error: " ++ (Expr.notPrettyShow leftValue) ++ " is not an available value in or operator")
		-- and运算，加入短路机制
		| operator == Expr.AndOperator	= let (leftValue,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList in
			if (Expr.checkWhetherBool leftValue == True)
				then
					if (leftValue == (Expr.BoolConstant False))
						then (leftValue,newGlobalVariable1)
						else let (rightValue,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
							if (Expr.checkWhetherBool rightValue == True)
								then (rightValue,newGlobalVariable2)
								else error ("Runtime Error: " ++ (Expr.notPrettyShow rightValue) ++ " is not an available value in amd operator")
				else error ("Runtime Error: " ++ (Expr.notPrettyShow leftValue) ++ " is not an available value in and operator")
		-- 加法运算
		| operator == Expr.PlusOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.plusConstant value1 value2,newGlobalVariable2)
		-- 减法运算
		| operator == Expr.MinusOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.minusConstant value1 value2,newGlobalVariable2)
		-- 乘法运算
		| operator == Expr.MultiplicationOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.multiplicationConstant value1 value2,newGlobalVariable2)
		-- 除法运算
		| operator == Expr.DivisionOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.divisionConstant value1 value2,newGlobalVariable2)
		-- 相等运算
		| operator == Expr.EqualOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.equalConstant value1 value2,newGlobalVariable2)
		-- 小于运算
		| operator == Expr.LessOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.lessConstant value1 value2,newGlobalVariable2)
		-- 小于等于运算
		| operator == Expr.LeqOperator =
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.leqConstant value1 value2,newGlobalVariable2)
		-- 大于运算
		| operator == Expr.GreatOperator = 
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.greatConstant value1 value2,newGlobalVariable2)
		-- 大于等于运算
		| operator == Expr.GeqOperator = 
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.geqConstant value1 value2,newGlobalVariable2)
		-- cons运算
		| operator == Expr.ConsOperator = 
			let (value1,newGlobalVariable1) = valueOfExpr expr1 globalVariable localVariable functionList ;
				(value2,newGlobalVariable2) = valueOfExpr expr2 newGlobalVariable1 localVariable functionList in
				(Expr.consConstant value1 value2,newGlobalVariable2)
		-- car运算
		| operator == Expr.CarOperator = let (value,newGlobalVariable) = valueOfExpr expr1 globalVariable localVariable functionList in (Expr.carConstant value,newGlobalVariable)
		-- cdr运算
		| operator == Expr.CdrOperator = let (value,newGlobalVariable) = valueOfExpr expr1 globalVariable localVariable functionList in (Expr.cdrConstant value,newGlobalVariable)
	
	-- 数组引用求值
	valueOfExpr (Expr.ArrayExpr var expr) globalVariable localVariable functionList =
		let (value,newGlobalVariable) = valueOfExpr expr globalVariable localVariable functionList; nowvar = visitVariable var newGlobalVariable localVariable in 
			if (Expr.checkConstantWhetherArray nowvar)
				then if (Expr.checkConstantWhetherInt value)
					then (Expr.visitArrayValue nowvar (Expr.convertConstantToInteger value),newGlobalVariable)
					else error ("Runtime Error: " ++ (Expr.notPrettyShow value) ++ " is not an available vector index")
				else error ("Runtime Error: " ++ (Expr.notPrettyShow nowvar) ++ " is not a vector")
	
	-- 函数表达式求值	
	valueOfExpr (Expr.FunctionExpr functionName numParm params) globalVariable localVariable functionList =	
		let (paramsValue,newGlobalVariable1) = getValueOfParams params globalVariable localVariable functionList ;
			(newGlobalVariable2,value) = runFunction (functionName,numParm,paramsValue,functionList,newGlobalVariable1) in
			(value,newGlobalVariable2)

	-- 获取函数参数值
	getValueOfParams :: [Expr.Expr] -> Map.Map Variable.Variable Expr.Constant -> Map.Map Variable.Variable Expr.Constant -> Map.Map (Variable.Variable,Integer) Function.Function -> ([Expr.Constant],Map.Map Variable.Variable Expr.Constant)
	getValueOfParams [] globalVariable localVariable functionList = ([],globalVariable)
	getValueOfParams (x:xs) globalVariable localVariable functionList = 
		let (value,newGlobalVariable1) = valueOfExpr x globalVariable localVariable functionList ;
			(valueList,newGlobalVariable2) = getValueOfParams xs newGlobalVariable1 localVariable functionList in
			(value:valueList,newGlobalVariable2)
