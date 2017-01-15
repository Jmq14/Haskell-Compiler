module ParseStatement where
	import qualified ParseExpr as ParseExpr;
	import qualified Expr as Expr;
	import qualified Tree as Tree
	import qualified Variable as Variable

	import qualified Debug.Trace as Trace

	-- 解析变量
	parseSetVariable :: [String] -> (Tree.Node,[String])
	parseSetVariable [] = error "Compile error: there should be variable identifier and an expression in variable setting"
	parseSetVariable (x:xs) = let (expr,newAhead) = ParseExpr.parseExpr xs in (Tree.SetVariableNode (Variable.parseVariable x) expr,newAhead)

	-- 解析If语句
	parseIfStatement :: [String] -> (Tree.Node,[String])
	parseIfStatement [] = error "Compile error: missing parameters of if statement"
	parseIfStatement xs = let (expr,newAhead1) = ParseExpr.parseExpr xs ; (branch1,newAhead2) = parseStatement newAhead1 ; (branch2,newAhead3) = parseStatement newAhead2 in (Tree.IfNode expr branch1 branch2,newAhead3)

	-- 解析while语句
	parseWhileStatement :: [String] -> (Tree.Node,[String])
	parseWhileStatement xs = let (expr,newAhead1) = ParseExpr.parseExpr xs ; (statement,newAhead2) = parseStatement newAhead1 in (Tree.WhileNode expr statement,newAhead2)

	-- 解析多条语句
	parseStatementList :: [String] -> (Tree.Node,[String])
	parseStatementList [] = error "Compile error: there is a missing \")\" in begin statement"
	parseStatementList (x:xs)
		| x == ")"		= (Tree.Nil,x:xs)
		| otherwise		= let (statement1,newAhead1) = parseStatement (x:xs) ; (statement2,newAhead2) = parseStatementList newAhead1 in (Tree.StatementListNode statement1 statement2,newAhead2)

	-- 解析print语句
	parsePrint :: [String] -> (Tree.Node,[String])
	parsePrint [] = error "Compile error: there is a mising \")\" in print statement"
	parsePrint (x:xs)
		| x == ")"		= (Tree.PrintNode (Expr.NewConstant (Expr.StringConstant "\n")),x:xs)
		| otherwise		= let (expr,newAhead) = ParseExpr.parseExpr (x:xs) in (Tree.PrintNode expr,newAhead)

	-- 解析创建数组语句
	parseMakeVector :: [String] -> (Tree.Node,[String])
	parseMakeVector [] = error "Compile error: there is a missing \")\" in make-vector statement"
	parseMakeVector (x:xs) = let var = Variable.parseVariable x ; (expr,newAhead) = ParseExpr.parseExpr xs in (Tree.MakeVectorNode var expr,newAhead)

	-- 解析数组赋值语句
	parseVectorSet :: [String] -> (Tree.Node,[String])
	parseVectorSet [] = error "Compile error: there is a missing \")\" in vector-set statement"
	parseVectorSet (x:xs) = let var = Variable.parseVariable x ; (expr1,newAhead1) = ParseExpr.parseExpr xs ; (expr2,newAhead2) = ParseExpr.parseExpr newAhead1 in (Tree.VectorSetNode var expr1 expr2,newAhead2)

	-- 解析返回语句
	parseReturn :: [String] -> (Tree.Node,[String])
	parseReturn [] = error "Compile error: return value undefined"
	parseReturn (x:xs) = let (expr,newAhead) = ParseExpr.parseExpr (x:xs) in (Tree.ReturnNode expr,newAhead)

	-- 解析括号开头的语句
	parseBracketStatement :: [String] -> (Tree.Node,[String])
	parseBracketStatement [] = error "Compile error: there is only \"(\" in the statement";
	parseBracketStatement (x:xs)
		| x == "set!"			= parseSetVariable xs
		| x == "if"				= parseIfStatement xs
		| x == "while"			= parseWhileStatement xs
		| x == "begin"			= parseStatementList xs
		| x == "print"			= parsePrint xs
		| x == "make-vector"	= parseMakeVector xs
		| x == "vector-set!"	= parseVectorSet xs
		| x == "return"			= parseReturn xs
		| otherwise				= error ("Compile error: unknown instruction " ++ x)

	-- 解析一条语句
	parseStatement :: [String]-> (Tree.Node,[String])
	parseStatement [] = (Tree.Nil,[]);
	parseStatement (x:xs)
		| x == "skip"	= (Tree.Nil,xs)
		| x == "("		= let (node,newAhead) = parseBracketStatement xs in
							if ((newAhead == []) || ((head newAhead) /= ")"))
								then error "Compile error: there is a missing \")\""
								else (node,tail newAhead)
		| otherwise		= (Tree.Nil,(x:xs))
