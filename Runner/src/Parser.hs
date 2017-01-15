module Parser where
	import qualified Tree as Tree;
	import qualified Expr as Expr;
	import qualified Function as Function;
	import qualified Variable as Variable;

	import qualified ParseFunction as ParseFunction;
	import qualified ParseExpr as ParseExpr;
	import qualified ParseStatement as ParseStatement;

	import qualified Data.List.Split as Split;
	import qualified Data.Map as Map;

	import qualified Debug.Trace as Trace

	-- �޳����ַ���
	filt :: [String] -> [String]
	filt [] = []
	filt (x:xs) =
		if (x == [])
			then filt xs
			else (x:(filt xs))

	-- �滻�Ʊ���Ϊ�ո�
	repl :: Char -> Char
	repl '\t' = ' '
	repl c = c

	-- ������ȡ
	convertBracket :: String -> String
	convertBracket [] = []
	convertBracket (x:xs)
		| x == '(' = ' ':x:' ':convertBracket xs
		| x == ')' = ' ':x:' ':convertBracket xs
		| otherwise = x:convertBracket xs 

	-- Step 3
	preSplitSpace :: String -> [String]
	preSplitSpace s = Split.splitOn " " $ convertBracket $ map repl s

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
		handle (r:rs) = preSplitSpace r ++ ["'" ++ head rs ++ "'"] ++ handle (tail rs)

	-- Step 1
	preSplitString :: [String] -> [String]
	preSplitString [] = []
	preSplitString (x:xs) = handle r ++ preSplitString xs where
		r = Split.splitOn "''" x
		handle [] = []
		handle (r:[]) = preSplitChar r
		handle (r:rs) = preSplitChar r ++ ["\"" ++ head rs ++ "\""] ++ handle (tail rs)

	-- Ԥ��ָ�������
	preSplit :: String -> [String]
	preSplit = filt . preSplitString . preSplitLine

	-- ��������
	parseProgram :: [String] -> (Map.Map (Variable.Variable,Integer) Function.Function)
	parseProgram s = ParseFunction.parseFunctionList s

	-- �����������ַ����б�
	parseOn :: [String] -> (Map.Map (Variable.Variable,Integer) Function.Function)
	parseOn s = parseProgram s
	
	-- ��������ں���
	myParse :: String -> (Map.Map (Variable.Variable,Integer) Function.Function)
	myParse s = parseOn (preSplit s)