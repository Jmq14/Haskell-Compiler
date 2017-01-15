module ParseExpr where
	import qualified Expr as Expr;
	import qualified Variable as Variable;

	import qualified Data.Ratio as Ratio;

	-- ���������������ʽ
	parseTwoExpr :: [String] -> (Expr.Expr,Expr.Expr,[String])
	parseTwoExpr x = let 
			(newExpr1 , newAhead1) = parseExpr x
			(newExpr2 , newAhead2) = parseExpr newAhead1
		in (newExpr1 , newExpr2 , newAhead2);

	-- ����not���ʽ
	parseNotExpr :: [String] -> (Expr.Expr,[String])
	parseNotExpr x	= let (newExpr,newAhead) = parseExpr x in (Expr.NewExpr Expr.NotOperator Expr.BoolType newExpr Expr.EmptyExpr,newAhead);

	-- ����and���ʽ
	parseAndExpr :: [String] -> (Expr.Expr,[String])
	parseAndExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.AndOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	-- ����or���ʽ
	parseOrExpr :: [String] -> (Expr.Expr,[String])
	parseOrExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.OrOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	-- �����ӷ����ʽ
	parsePlusExpr :: [String] -> (Expr.Expr,[String])
	parsePlusExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.PlusOperator Expr.FloatType newExpr1 newExpr2,newAhead);

	-- �����������ʽ
	parseMinusExpr :: [String] -> (Expr.Expr,[String])
	parseMinusExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.MinusOperator Expr.FloatType newExpr1 newExpr2,newAhead);

	-- �����˷����ʽ
	parseMultiplicationExpr :: [String] -> (Expr.Expr,[String])
	parseMultiplicationExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.MultiplicationOperator Expr.FloatType newExpr1 newExpr2,newAhead);

	-- �����������ʽ
	parseDivisionExpr :: [String] -> (Expr.Expr,[String])
	parseDivisionExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.DivisionOperator Expr.FloatType newExpr1 newExpr2,newAhead);

	-- ������ȱ��ʽ
	parseEqualExpr :: [String] -> (Expr.Expr,[String])
	parseEqualExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.EqualOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	-- ����С�ڱ��ʽ
	parseLessExpr :: [String] -> (Expr.Expr,[String])
	parseLessExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.LessOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	-- ����С�ڵ��ڱ��ʽ
	parseLeqExpr :: [String] -> (Expr.Expr,[String])
	parseLeqExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.LeqOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	-- �������ڱ��ʽ
	parseGreatExpr :: [String] -> (Expr.Expr,[String])
	parseGreatExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.GreatOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	-- �������ڵ��ڱ��ʽ
	parseGeqExpr :: [String] -> (Expr.Expr,[String])
	parseGeqExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.GeqOperator Expr.BoolType newExpr1 newExpr2,newAhead);

	-- ����cons���ʽ
	parseConsExpr :: [String] -> (Expr.Expr,[String])
	parseConsExpr x = let (newExpr1,newExpr2,newAhead) = parseTwoExpr x in (Expr.NewExpr Expr.ConsOperator (Expr.PairType (Expr.getExprType newExpr1) (Expr.getExprType newExpr2)) newExpr1 newExpr2,newAhead);

	-- ����car���ʽ
	parseCarExpr :: [String] -> (Expr.Expr,[String])
	parseCarExpr x = let (newExpr,newAhead) = parseExpr x in (Expr.NewExpr Expr.CarOperator (Expr.getPairLeftType newExpr) newExpr Expr.EmptyExpr,newAhead);

	-- ����cdr���ʽ
	parseCdrExpr :: [String] -> (Expr.Expr,[String])
	parseCdrExpr x = let (newExpr,newAhead) = parseExpr x in (Expr.NewExpr Expr.CdrOperator (Expr.getPairRightType newExpr) newExpr Expr.EmptyExpr,newAhead);

	-- �����������ñ��ʽ
	parseVectorRefExpr :: [String] -> (Expr.Expr,[String])
	parseVectorRefExpr [] = error ("Compile error: there is missing parameters in vector reference")
	parseVectorRefExpr (x:xs) = let var = Variable.parseVariable x ; (expr,newAhead) = parseExpr xs in (Expr.ArrayExpr var expr,newAhead)

	-- ����ʵ��
	parseFloat :: [String] -> (Expr.Expr,[String])
	parseFloat (x:xs) = (Expr.NewConstant (Expr.FloatConstant (Ratio.approxRational (read x :: Double) 0.000001)),xs); 

	-- �����ַ�
	parseChar :: String -> Expr.Expr
	parseChar ('\'':x:'\'':[]) = Expr.NewConstant (Expr.CharConstant x)
	parseChar x = error ("Compile error: " ++ (show x) ++ " is now an available Char")

	-- �����ַ���
	parseString :: String -> Expr.Expr
	parseString s =
		if ((head s) == '\"' && (last s) == '\"')
			then Expr.NewConstant (Expr.StringConstant (init (tail s)))
			else error ("Compile error: there is a missing \"\"\" in the end of the string")

	-- ������������
	parseExprList :: [String] -> (Integer,[Expr.Expr],[String])
	parseExprList [] = error "Compile error: there is a missing \")\""
	parseExprList (x:xs) =
		if (x == ")")
			then (0,[],x:xs)
			else let (expr,newAhead1) = parseExpr (x:xs) ;
					 (numVar,exprList,newAhead2) = parseExprList newAhead1 in
					 (numVar+1,expr:exprList,newAhead2)

	-- ������������
	parseFunctionRef :: [String] -> (Expr.Expr,[String])
	parseFunctionRef [] = error "Compile error: no function name in function call"
	parseFunctionRef (x:xs) =
		let functionName = Variable.parseVariable x ;
			(numVar,exprList,newAhead) = parseExprList xs in
			(Expr.FunctionExpr functionName numVar exprList,newAhead)

	-- ���������ſ�ͷ�ı��ʽ
	parseBracketExpr :: [String] -> (Expr.Expr,[String])
	parseBracketExpr (x:xs)
		| x == "not"			= parseNotExpr xs
		| x == "and"			= parseAndExpr xs
		| x == "or"				= parseOrExpr xs
		| x == "+"				= parsePlusExpr xs
		| x == "-"				= parseMinusExpr xs
		| x == "*"				= parseMultiplicationExpr xs
		| x == "/"				= parseDivisionExpr xs
		| x == "="				= parseEqualExpr xs
		| x == "<"				= parseLessExpr xs
		| x == "<="				= parseLeqExpr xs
		| x == ">"				= parseGreatExpr xs
		| x == ">="				= parseGeqExpr xs
		| x == "cons"			= parseConsExpr xs
		| x == "car"			= parseCarExpr xs
		| x == "cdr"			= parseCdrExpr xs
		| x == "vector-ref"		= parseVectorRefExpr xs
		| otherwise				= parseFunctionRef (x:xs)

	-- �������ʽ
	parseExpr :: [String] -> (Expr.Expr,[String])
	parseExpr [] = error ("Compile error: missing expression");
	parseExpr (x:xs) 
		| x == "True"	= (Expr.NewConstant (Expr.BoolConstant True),xs)
		| x == "False"	= (Expr.NewConstant (Expr.BoolConstant False),xs)
		| x == "("		= let (expr,newAhead) = parseBracketExpr xs in
							if ((newAhead == []) || ((head newAhead) /= ")"))
								then error ("Compile error: there is a missing \")\"")
								else (expr,tail newAhead)
		| otherwise		=
			if ((head x) >= '0' && (head x) <= '9')
				then parseFloat (x:xs)
				else
					if (head x == '\'')
						then (parseChar x,xs)
						else
							if (head x == '\"')
								then (parseString x,xs)
								else
									let var = Variable.parseVariable x in
										if (var == Variable.ErrorVariable)
											then (Expr.EmptyExpr,(x:xs))
											else (Expr.NewConstant (Expr.VariableConstant var),xs)
