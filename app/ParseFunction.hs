module ParseFunction where
	import qualified Function as Function
	import qualified Variable as Variable

	import qualified ParseStatement as ParseStatement

	import qualified Data.Map as Map

	import qualified Debug.Trace as Trace;

	parseVariableList :: [String] -> (Integer,[Variable.Variable],[String])
	parseVariableList (x:xs) = 
		if (x == ")")
			then (0,[],xs)
			else let (varLen,varList,newAhead) = parseVariableList xs in (1+varLen,(Variable.parseVariable x):varList,newAhead)

	parseFunctionIniside :: [String] -> (Variable.Variable,Integer,[Variable.Variable],[String])
	parseFunctionIniside ("(":(x:xs)) = let function = Variable.parseVariable x ; (numVar,varList,newAhead) = parseVariableList xs in (function,numVar,varList,newAhead)
	parseFunctionIniside _ = error "There is a missing \"(\""

	parseFunction :: [String] -> (Variable.Variable,Integer,Function.Function,[String])
	parseFunction (x:(xs:xss)) =
		if (x== "(" && xs == "define")
			then let (function,numVar,varList,newAhead1) = parseFunctionIniside xss ; (node,newAhead2) = ParseStatement.parseStatement newAhead1 in 
				if ((head newAhead2) == ")")
					then (function,numVar,Function.NewFunction function numVar varList node,tail newAhead2) 
					else error "The function should end with \")\""
			else error "The function should begin with \"(define\""
	parseFunction _ = error "There are extra words in the last"

	parseFunctionList :: [String] -> (Map.Map (Variable.Variable,Integer) Function.Function)
	parseFunctionList [] = Map.empty
	parseFunctionList x = let (function,numVar,finalFunc,newAhead) = parseFunction x ; functionList = parseFunctionList newAhead in Map.insert (function,numVar) finalFunc functionList;
