module ParseFunction where
	import qualified Function as Function
	import qualified Variable as Variable

	import qualified ParseStatement as ParseStatement

	import qualified Data.Map as Map

	import qualified Debug.Trace as Trace;
	
	-- 解析函数参数变量名列表
	parseVariableList :: [String] -> (Integer,[Variable.Variable],[String])
	parseVariableList (x:xs) = 
		if (x == ")")
			then (0,[],xs)
			else let (varLen,varList,newAhead) = parseVariableList xs in (1+varLen,(Variable.parseVariable x):varList,newAhead)

	-- 解析函数名和参数列表
	parseFunctionIniside :: [String] -> (Variable.Variable,Integer,[Variable.Variable],[String])
	parseFunctionIniside ("(":(x:xs)) = let function = Variable.parseVariable x ; (numVar,varList,newAhead) = parseVariableList xs in (function,numVar,varList,newAhead)
	parseFunctionIniside _ = error "Compile error: there is a missing \"(\" in the definition of function"

	-- 解析函数
	parseFunction :: [String] -> (Variable.Variable,Integer,Function.Function,[String])
	parseFunction (x:(xs:xss)) =
		if (x== "(" && xs == "define")
			then let (function,numVar,varList,newAhead1) = parseFunctionIniside xss ; (node,newAhead2) = ParseStatement.parseStatement newAhead1 in 
				if ((head newAhead2) == ")")
					then (function,numVar,Function.NewFunction function numVar varList node,tail newAhead2) 
					else error "Compile error: the function should end with \")\""
			else error "Compile error: the function should begin with \"(define\""
	parseFunction _ = error "Compile error: there are some extra words in the last"

	-- 解析很多个函数
	parseFunctionList :: [String] -> (Map.Map (Variable.Variable,Integer) Function.Function)
	parseFunctionList [] = Map.empty
	parseFunctionList x = let (function,numVar,finalFunc,newAhead) = parseFunction x ; functionList = parseFunctionList newAhead in Map.insert (function,numVar) finalFunc functionList;
