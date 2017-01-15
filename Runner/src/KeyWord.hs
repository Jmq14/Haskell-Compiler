module KeyWord where
	-- �ؼ����б�
	keywords = ["True","False","not","and","or","cons","car","cdr","set!","skip","if","while","begin","vector-ref","make-vector","vector-set!","print","define","return"]

	-- ���õı����ַ�
	variableChar = ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ ['0'..'9'];

	-- Statment�Ĺؼ����б�
	statementKeywords = ["set!","if","while","begin","make-vector","vector-set!","print","return"]

	-- ���ĳ���ַ����Ƿ�������һ���ַ��������г��ֹ�
	insideOrNot :: String -> [String] -> Bool
	insideOrNot s [] = False
	insideOrNot s (x:xs) =
		if (s == x)
			then True
			else insideOrNot s xs
