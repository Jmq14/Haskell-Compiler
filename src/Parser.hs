module Parser where
	import Tree;
	import Expr;

	import Data.List.Split;

	preSplit :: String -> [String]
	preSplit s = splitOn " " s; 

	parseTwoExpr :: [String] -> (Expr,Expr,[String])
	parseTwoExpr x = let 
			(newExpr1 , newAhead1) = parseExpr x
			(newExpr2 , newAhead2) = parseExpr newAhead1
		in (newExpr1 , newExpr2 , newAhead2);

	parseNotExpr :: [String] -> (Expr,[String])
	parseNotExpr x	= let (newExpr,newAhead) = parseExpr x in (NewExpr NotOperator BoolType newExpr EmptyExpr,newAhead);

	parseAndExpr :: [String] -> (Expr,[String])
	parseAndExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr AndOperator BoolType newExpr1 newExpr2,newAhead);

	parseOrExpr :: [String] -> (Expr,[String])
	parseOrExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr OrOperator BoolType newExpr1 newExpr2,newAhead);

	parsePlusExpr :: [String] -> (Expr,[String])
	parsePlusExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr PlusOperator FloatType newExpr1 newExpr2,newAhead);

	parseMinusExpr :: [String] -> (Expr,[String])
	parseMinusExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr MinusOperator FloatType newExpr1 newExpr2,newAhead);

	parseMultiplicationExpr :: [String] -> (Expr,[String])
	parseMultiplicationExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr MultiplicationOperator FloatType newExpr1 newExpr2,newAhead);

	parseDivisionExpr :: [String] -> (Expr,[String])
	parseDivisionExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr DivisionOperator FloatType newExpr1 newExpr2,newAhead);

	parseEqualExpr :: [String] -> (Expr,[String])
	parseEqualExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr EqualOperator BoolType newExpr1 newExpr2,newAhead);

	parseLessExpr :: [String] -> (Expr,[String])
	parseLessExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr LessOperator BoolType newExpr1 newExpr2,newAhead);

	parseLeqExpr :: [String] -> (Expr,[String])
	parseLeqExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr LeqOperator BoolType newExpr1 newExpr2,newAhead);

	parseGreatExpr :: [String] -> (Expr,[String])
	parseGreatExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr GreatOperator BoolType newExpr1 newExpr2,newAhead);

	parseGeqExpr :: [String] -> (Expr,[String])
	parseGeqExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr GeqOperator BoolType newExpr1 newExpr2,newAhead);

	parseFloat :: [String] -> (Expr,[String])
	parseFloat (x:xs) = (NewConstant (FloatConstant (read x :: Float)),xs); 

	parseExpr :: [String] -> (Expr,[String])
	parseExpr [] = (EmptyExpr,[]);
	parseExpr (x:xs) 
		| x == "True"	= (NewConstant (BoolConstant True),xs)
		| x == "False"	= (NewConstant (BoolConstant False),xs)
		| x == "not"	= parseNotExpr xs
		| x == "and"	= parseAndExpr xs
		| x == "or"		= parseOrExpr xs
		| x == "+"		= parsePlusExpr xs
		| x == "-"		= parseMinusExpr xs
		| x == "*"		= parseMultiplicationExpr xs
		| x == "/"		= parseDivisionExpr xs
		| x == "="		= parseEqualExpr xs
		| x == "<"		= parseLessExpr xs
		| x == "<="		= parseLeqExpr xs
		| x == ">"		= parseGreatExpr xs
		| x == ">="		= parseGeqExpr xs
		| otherwise		=
			if ((head x) >= '0' && (head x) <= '9')
				then parseFloat (x:xs)
				else (EmptyExpr,(x:xs))

	parseOn :: [String] -> (Expr,[String]);
	parseOn s = parseExpr s;
	
	myParse :: String -> Expr;
	myParse s = let (result,ahead) = parseOn (preSplit s) in result;
