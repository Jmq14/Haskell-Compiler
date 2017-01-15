module KeyWord where
	-- 关键词列表
	keywords = ["True","False","not","and","or","cons","car","cdr","set!","skip","if","while","begin","vector-ref","make-vector","vector-set!","print","define","return"]

	-- 可用的变量字符
	variableChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9'];

	-- Statment的关键字列表
	statementKeywords = ["set!","if","while","begin","make-vector","vector-set!","print","return"]

	-- 检查某个字符串是否在另外一个字符串数组中出现过
	insideOrNot :: String -> [String] -> Bool
	insideOrNot s [] = False
	insideOrNot s (x:xs) =
		if (s == x)
			then True
			else insideOrNot s xs
