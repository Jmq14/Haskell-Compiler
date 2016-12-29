module Variable where
	data Variable = NewVariable String | ErrorVariable deriving (Show, Eq, Ord);

	parseVariable :: String -> Variable
	parseVariable = undefined
