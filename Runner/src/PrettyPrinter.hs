module PrettyPrinter where
    import Data.Char
    import System.IO

    --参数为asttree的数列表，输出美化后的asttree
    prettyPrinter [] = do putStr ""
    prettyPrinter (x:xs) = do
        let y = mySplit $ show x
        astPrinter 0 0 y
        putStrLn ""
        prettyPrinter xs

    --n0为缩进个数，n1表示(x:xs)是否为参数，(x:xs)为要输出的内容
    astPrinter n0 n1 (x:xs) = do
        if x == "StatementListNode"
            then do
                if n0 == 1
                    then do
                        putStrLn "  StatementListNode"
                        astPrinter (n0 + 1) 0 xs
                else do
                    putStr ""
                    astPrinter (n0 - 1) 0 xs
        else if all == [x]
            then do
                if '%' `elem` x
                    then printLine (n0 + n1) (rationalToFractional (words x))
                else printLine (n0 + n1) x
        else do
            astPrinter (n0 + n1) 0 all
        if (x /= "StatementListNode") && (xs /= [])
            then astPrinter n0 1 xs
        else putStr ""
        where all = mySplit x

    --n为缩进个数，缩进完成后输出x
    printLine n x = do
        if n > 0
            then do
                putStr "  "
                printLine (n - 1) x
        else putStrLn x

    --根据空格分割字符串（不分割括号内的空格）
    mySplit :: String -> [String]
    mySplit [] = []
    mySplit (x:xs) =
        if x == '('
            then mySplit1 1 False [] xs
        else if x == '['
            then mySplit1 0 True [x] xs
        else mySplit1 0 False [x] xs

    mySplit1 :: Int -> Bool -> String -> String -> [String]
    mySplit1 0 _ x [] = [x]
    mySplit1 0 isArray x (y:ys) =
        if y == '('
            then mySplit1 1 isArray x ys
        else if (y == ' ') && (a /= '\"') && (a /= '%') && (not (isDigit a)) && (not ((a == '(') && (isDigit (head b)))) && (not isArray)
            then (x:(mySplit1 0 isArray [] ys))
        else if (y == '[')
            then mySplit1 0 True (x ++ [y]) ys
        else if (y == ']')
            then mySplit1 0 False (x ++ [y]) ys
        else mySplit1 0 isArray (x ++ [y]) ys
        where (a:b) = ys
    mySplit1 n isArray x (y:ys) =
        if (y == '(')
            then mySplit1 (n + 1) isArray (x ++ [y]) ys
        else if (y == ')') && (n == 1)
            then mySplit1 0 isArray x ys
        else if y == ')'
            then mySplit1 (n - 1) isArray (x ++ [y]) ys
        else if (y == '[')
            then mySplit1 n True (x ++ [y]) ys
        else if (y == ']')
            then mySplit1 n False (x ++ [y]) ys
        else mySplit1 n isArray (x ++ [y]) ys

    --将rational的表达转化为fractional的表达
    rationalToFractional [] = ""
    rationalToFractional [a] = a
    rationalToFractional (a:b:c) = 
        if b == "%"
            then (show ((read a) / (read denominator))) ++ (drop (length denominator) headC) ++ (' ':(rationalToFractional (tail c)))
        else a ++ (' ':(rationalToFractional (b:c)))
        where headC = head c
              denominator = getNum headC

    getNum [] = []
    getNum (x:xs) =
        if isDigit x
            then (x:(getNum xs))
        else []