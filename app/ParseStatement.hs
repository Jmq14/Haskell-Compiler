module ParseStatement where
	import qualified ParseExpr as ParseExpr;
	import qualified Expr as Expr;
	import qualified Tree as Tree
	import qualified Variable as Variable

	import qualified Debug.Trace as Trace

	parseSetVariable :: [String] -> (Tree.Node,[String])
	parseSetVariable [] = (Tree.ErrorNode,[])
	parseSetVariable (x:xs) = let (expr,newAhead) = ParseExpr.parseExpr xs in (Tree.SetVariableNode (Variable.parseVariable x) expr,newAhead)

	parseIfStatement :: [String] -> (Tree.Node,[String])
	parseIfStatement [] = (Tree.ErrorNode,[])
	parseIfStatement xs = let (expr,newAhead1) = ParseExpr.parseExpr xs ; (branch1,newAhead2) = parseStatement newAhead1 ; (branch2,newAhead3) = parseStatement newAhead2 in (Tree.IfNode expr branch1 branch2,newAhead3)

	parseWhileStatement :: [String] -> (Tree.Node,[String])
	parseWhileStatement xs = let (expr,newAhead1) = ParseExpr.parseExpr xs ; (statement,newAhead2) = parseStatement newAhead1 in (Tree.WhileNode expr statement,newAhead2)

	parseStatementList :: [String] -> (Tree.Node,[String])
	parseStatementList [] = (Tree.ErrorNode,[])
	parseStatementList (x:xs)
		| x == ")"		= (Tree.Nil,x:xs)
		| otherwise		= let (statement1,newAhead1) = parseStatement (x:xs) ; (statement2,newAhead2) = parseStatementList newAhead1 in (Tree.StatementListNode statement1 statement2,newAhead2)

	parsePrint :: [String] -> (Tree.Node,[String])
	parsePrint [] = (Tree.ErrorNode,[])
	parsePrint (x:xs)
		| x == ")"		= (Tree.Nil,x:xs)
		| otherwise		= let (expr,newAhead) = ParseExpr.parseExpr (x:xs) in (Tree.PrintNode expr,newAhead)

	parseMakeVector :: [String] -> (Tree.Node,[String])
	parseMakeVector [] = (Tree.ErrorNode,[])
	parseMakeVector (x:xs) = let var = Variable.parseVariable x ; (expr,newAhead) = ParseExpr.parseExpr xs in (Tree.MakeVectorNode var expr,newAhead)

	parseVectorSet :: [String] -> (Tree.Node,[String])
	parseVectorSet [] = (Tree.ErrorNode,[])
	parseVectorSet (x:xs) = let var = Variable.parseVariable x ; (expr1,newAhead1) = ParseExpr.parseExpr xs ; (expr2,newAhead2) = ParseExpr.parseExpr newAhead1 in (Tree.VectorSetNode var expr1 expr2,newAhead2)

	parseReturn :: [String] -> (Tree.Node,[String])
	parseReturn [] = error "sb"
	parseReturn (x:xs) = let (expr,newAhead) = ParseExpr.parseExpr (x:xs) in (Tree.ReturnNode expr,newAhead)

	parseBracketStatement :: [String] -> (Tree.Node,[String])
	parseBracketStatement [] = (Tree.ErrorNode,[]);
	parseBracketStatement (x:xs)
		| x == "set!"			= parseSetVariable xs
		| x == "if"				= parseIfStatement xs
		| x == "while"			= parseWhileStatement xs
		| x == "begin"			= parseStatementList xs
		| x == "print"			= parsePrint xs
		| x == "make-vector"	= parseMakeVector xs
		| x == "vector-set!"	= parseVectorSet xs
		| x == "return"			= parseReturn xs
		| otherwise				= (Tree.ErrorNode,xs)

	parseStatement :: [String]-> (Tree.Node,[String])
	parseStatement [] = (Tree.Nil,[]);
	parseStatement (x:xs)
		| x == "skip"	= (Tree.Nil,xs)
		| x == "("		= let (node,newAhead) = parseBracketStatement xs in
							if ((newAhead == []) || ((head newAhead) /= ")"))
								then error "There is a missing \")\""
								else (node,tail newAhead)
		| otherwise		= (Tree.Nil,(x:xs))
