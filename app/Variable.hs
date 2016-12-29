module Variable where
	import qualified KeyWord as KeyWord;

	data Variable = NewVariable String | ErrorVariable deriving (Show, Eq, Ord);

	parseVariable :: String -> Variable
	parseVariable = undefined
