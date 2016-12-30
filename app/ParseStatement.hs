module ParseStatement where
	import qualified ParseExpr as ParseExpr;
	import qualified Expr as Expr;
	import qualified Tree as Tree
	import qualified Variable as Variable

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
		| x == ")"		= (Tree.Nil,xs)
		| otherwise		= let (statement1,newAhead1) = parseStatement (x:xs) ; (statement2,newAhead2) = parseStatementList newAhead1 in (Tree.StatementListNode statement1 statement2,newAhead2)

	parsePrint :: [String] -> (Tree.Node,[String])
	parsePrint [] = (Tree.ErrorNode,[])
	parsePrint (x:xs)
		| x == ")"		= (Tree.Nil,xs)
		| otherwise		= let (expr,newAhead) = ParseExpr.parseExpr (x:xs) in (Tree.PrintNode expr,newAhead)

	parseBracketStatement :: [String] -> (Tree.Node,[String])
	parseBracketStatement [] = undefined;
	parseBracketStatement (x:xs)
		| x == "set!"	= parseSetVariable xs
		| x == "if"		= parseIfStatement xs
		| x == "while"	= parseWhileStatement xs
		| x == "begin"	= parseStatementList xs
		| x == "print"	= parsePrint xs

	parseStatement :: [String]-> (Tree.Node,[String])
	parseStatement [] = (Tree.Nil,[]);
	parseStatement (x:xs)
		| x == "skip"	= undefined
		| x == "("		= let (node,newAhead) = parseBracketStatement xs in
							if ((newAhead == []) || ((head newAhead) /= ")"))
								then (Tree.ErrorNode,newAhead)
								else (node,tail newAhead)
		| otherwise		= (Tree.Nil,(x:xs))
