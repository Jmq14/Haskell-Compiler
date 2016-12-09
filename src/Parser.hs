module Parser where
	import Tree;
	import Expr;

	import Data.List.Split;

	preSplit :: String -> [String]
	preSplit s = splitOn " " s; 

	parseSecondPartofTwoExpr :: Expr -> [String] -> (Expr,Expr,[String])
	parseSecondPartofTwoExpr newExpr1 x = let (newExpr2 , newAhead) = parseExpr x in (newExpr1 , newExpr2 , newAhead);

	parseFirstPartOfTwoExpr :: [String] -> (Expr,Expr,[String])
	parseFirstPartOfTwoExpr x = let (newExpr , newAhead) = parseExpr x in parseSecondPartofTwoExpr newExpr newAhead;

	parseTwoExpr :: [String] -> (Expr,Expr,[String])
	parseTwoExpr x = parseFirstPartOfTwoExpr x;

	parseNotExpr :: [String] -> (Expr,[String])
	parseNotExpr x	= let (newExpr,newAhead) = parseExpr x in (NewExpr NotOperator BoolType newExpr EmptyExpr,newAhead);

	parseOrExpr :: [String] -> (Expr,[String])
	parseOrExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr OrOperator BoolType newExpr1 newExpr2,newAhead);

	parseAndExpr :: [String] -> (Expr,[String])
	parseAndExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr AndOperator BoolType newExpr1 newExpr2,newAhead);

	parseExpr :: [String] -> (Expr,[String])
	parseExpr [] = (EmptyExpr,[]);
	parseExpr (x:xs) 
		| x == "True"	= (BoolConstant True,xs)
		| x == "False"	= (BoolConstant False,xs)
		| x == "not"	= parseNotExpr xs
		| x == "and"	= parseAndExpr xs
		| x == "or"		= parseOrExpr xs
		| otherwise		= (EmptyExpr,xs);

	parseOn :: [String] -> (Expr,[String]);
	parseOn s = parseExpr s;
	
	parse :: String -> Expr;
	parse s = let (result,ahead) = parseOn (preSplit s) in result;
