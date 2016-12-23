module Parser where
	import qualified Tree as Tree;
	import qualified Expr as Expr;

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

	parseTwoExpr :: [String] -> (Expr.Expr,Expr.Expr,[String])
	parseTwoExpr x = let 
			(newExpr1 , newAhead1) = parseExpr x
			(newExpr2 , newAhead2) = parseExpr newAhead1
		in (newExpr1 , newExpr2 , newAhead2);

	parseNotExpr :: [String] -> (Expr.Expr,[String])
	parseNotExpr x	= let (newExpr,newAhead) = parseExpr x in (Expr.NewExpr Expr.NotOperator Expr.BoolType newExpr Expr.EmptyExpr,newAhead);

	parseAndExpr :: [String] -> (Expr.Expr,[String])
	parseAndExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.AndOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	parseOrExpr :: [String] -> (Expr.Expr,[String])
	parseOrExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.OrOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	parsePlusExpr :: [String] -> (Expr.Expr,[String])
	parsePlusExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.PlusOperator Expr.FloatType newExpr1 newExpr2,newAhead);

	parseMinusExpr :: [String] -> (Expr.Expr,[String])
	parseMinusExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.MinusOperator Expr.FloatType newExpr1 newExpr2,newAhead);

	parseMultiplicationExpr :: [String] -> (Expr.Expr,[String])
	parseMultiplicationExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.MultiplicationOperator Expr.FloatType newExpr1 newExpr2,newAhead);

	parseDivisionExpr :: [String] -> (Expr.Expr,[String])
	parseDivisionExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.DivisionOperator Expr.FloatType newExpr1 newExpr2,newAhead);

	parseEqualExpr :: [String] -> (Expr.Expr,[String])
	parseEqualExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.EqualOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	parseLessExpr :: [String] -> (Expr.Expr,[String])
	parseLessExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.LessOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	parseLeqExpr :: [String] -> (Expr.Expr,[String])
	parseLeqExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.LeqOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	parseGreatExpr :: [String] -> (Expr.Expr,[String])
	parseGreatExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.GreatOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	parseGeqExpr :: [String] -> (Expr.Expr,[String])
	parseGeqExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.GeqOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	parseConsExpr :: [String] -> (Expr.Expr,[String])
	parseConsExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.ConsOperator (Expr.PairType (Expr.getExprType newExpr1) (Expr.getExprType newExpr2)) newExpr1 newExpr2,newAhead);

	parseCarExpr :: [String] -> (Expr.Expr,[String])
	parseCarExpr x = let (newExpr,newAhead) = parseExpr x in (Expr.NewExpr Expr.CarOperator (Expr.getPairLeftType newExpr) newExpr Expr.EmptyExpr,newAhead);

	parseCdrExpr :: [String] -> (Expr.Expr,[String])
	parseCdrExpr x = let (newExpr,newAhead) = parseExpr x in (Expr.NewExpr Expr.CdrOperator (Expr.getPairRightType newExpr) newExpr Expr.EmptyExpr,newAhead);

	parseFloat :: [String] -> (Expr.Expr,[String])
	parseFloat (x:xs) = (Expr.NewConstant (Expr.FloatConstant (read x :: Float)),xs); 

	parseExpr :: [String] -> (Expr.Expr,[String])
	parseExpr [] = (Expr.EmptyExpr,[]);
	parseExpr (x:xs) 
		| x == "True"	= (Expr.NewConstant (Expr.BoolConstant True),xs)
		| x == "False"	= (Expr.NewConstant (Expr.BoolConstant False),xs)
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
				else (Expr.EmptyExpr,(x:xs))

	parseOn :: [String] -> (Expr.Expr,[String]);
	parseOn s = parseExpr s;
	
	myParse :: String -> Expr.Expr;
	myParse s = let (result,ahead) = parseOn (preSplit s) in result;
