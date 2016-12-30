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

	-- Step 3
	preSplitSpace :: String -> [String]
	preSplitSpace s = Split.splitOn " " (map repl s)

	-- Step 0
	preSplitLine :: String -> [String]
	preSplitLine = Split.splitOn "\n"

	-- Step 2
	preSplitChar :: String -> [String]
	preSplitChar [] = []
	preSplitChar x = handle r where
		r = Split.splitOn "'" x
		handle [] = []
		handle (r:[]) = preSplitSpace r
		handle (r: rs) = preSplitSpace r ++ ["'" ++ head rs ++ "'"] ++ handle (tail rs)

	-- Step 1
	preSplitString :: [String] -> [String]
	preSplitString [] = []
	preSplitString (x:xs) = handle r ++ preSplitString xs where
		r = Split.splitOn "''" x
		handle [] = []
		handle (r:[]) = preSplitChar r
		handle (r: rs) = preSplitChar r ++ ["''" ++ head rs ++ "''"] ++ handle (tail rs)

	preSplit :: String -> [String]
	preSplit = filt . preSplitString . preSplitLine

	parseProgram :: [String] -> (Tree.Node,[String])
	parseProgram s = ParseStatement.parseStatement s

	parseOn :: [String] -> (Expr.Expr,[String])
	parseOn s = ParseExpr.parseExpr s
	
	myParse :: String -> Expr.Expr
	myParse s = let (result,ahead) = parseOn (preSplit s) in result
