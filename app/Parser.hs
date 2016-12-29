module Parser where
	import qualified Tree as Tree;
	import qualified Expr as Expr;
	import qualified ParseExpr as ParseExpr;
	import qualified ParseStatement as ParseStatement;

	import qualified Data.List.Split as Split;
	import qualified Data.Map as Map;

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

	parseProgram :: [String] -> (Tree.Node,[String])
	parseProgram s = ParseStatement.parseStatement s;

	parseOn :: [String] -> (Expr.Expr,[String]);
	parseOn s = ParseExpr.parseExpr s;
	
	myParse :: String -> Expr.Expr;
	myParse s = let (result,ahead) = parseOn (preSplit s) in result;
