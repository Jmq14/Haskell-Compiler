module Parser where
	import Tree;
	import Expr;

	import qualified Data.List.Split as Split;

	filt :: [String] -> [String]
	filt [] = []
	filt (x:xs) =
		if (x == [])
			then filt xs
			else (x:(filt xs))

	repl :: Char -> Char
	repl '\t' = ' '
	repl c = c

	preSplit2 :: [String] -> [String]
	preSplit2 [] = []
	preSplit2 (x:xs) = (Split.splitOn " " (map repl x)) ++ (preSplit2 xs)

	preSplit :: String -> [String]
	preSplit s = filt (preSplit2 (Split.splitOn "\n" s));

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

	parseConsExpr :: [String] -> (Expr,[String])
	parseConsExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (NewExpr ConsOperator (PairType (Expr.getExprType newExpr1) (Expr.getExprType newExpr2)) newExpr1 newExpr2,newAhead);

	parseCarExpr :: [String] -> (Expr,[String])
	parseCarExpr x = let (newExpr,newAhead) = parseExpr x in (NewExpr CarOperator (Expr.getPairLeftType newExpr) newExpr EmptyExpr,newAhead);

	parseCdrExpr :: [String] -> (Expr,[String])
	parseCdrExpr x = let (newExpr,newAhead) = parseExpr x in (NewExpr CdrOperator (Expr.getPairRightType newExpr) newExpr EmptyExpr,newAhead);

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
		| x == "cons"	= parseConsExpr xs
		| x == "car"	= parseCarExpr xs
		| x == "cdr"	= parseCdrExpr xs
		| otherwise		=
			if ((head x) >= '0' && (head x) <= '9')
				then parseFloat (x:xs)
				else (EmptyExpr,(x:xs))

	parseOn :: [String] -> (Expr,[String]);
	parseOn s = parseExpr s;
	
	myParse :: String -> Expr;
	myParse s = let (result,ahead) = parseOn (preSplit s) in result;
