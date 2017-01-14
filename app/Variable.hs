module Variable where
	import qualified KeyWord as KeyWord;

	import qualified Data.Map as Map;

	data Variable = NewVariable String | ErrorVariable deriving (Show, Eq, Ord);

	elementIn :: Eq a => a -> [a] -> Bool
	elementIn x xs = foldl (||) False (map (\y -> y == x) xs)

	parseVariable :: String -> Variable
	parseVariable s =
		if ((elementIn s KeyWord.keywords) || (elementIn False (map (\x -> elementIn x KeyWord.variableChar) s)) || (elementIn (head s) ['0'..'9']))
			then error ("Compile Error: " ++ s ++ " is not an avaliable variable name")
			else NewVariable s

