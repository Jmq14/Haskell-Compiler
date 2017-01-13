module PrettyPrinter where
    import Data.Char
    import System.IO

    prettyPrinter [] = do putStr ""
    prettyPrinter (x:xs) = do
        let y = mySplit $ show x
        astPrinter 0 0 y
        putStrLn ""
        prettyPrinter xs

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
            then printLine (n0 + n1) x
        else do
            astPrinter (n0 + n1) 0 all
        if (x /= "StatementListNode") && (xs /= [])
            then astPrinter n0 1 xs
        else putStr ""
        where all = mySplit x

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