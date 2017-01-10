module Lib where
    import System.Environment
    import System.Exit
    import System.IO
    import GHC.IO.Handle

    import qualified Parser as Parser
    import qualified Expr as Expr
    import qualified Tree as Tree
    import qualified Variable as Variable
    import qualified ParseExpr as ParseExpr
    import qualified Run as Run

    import qualified Data.Map as Map

    analyzeArgs :: [String] -> Map.Map String String
    analyzeArgs [] = Map.empty
    analyzeArgs (x:[]) = error "Invalid arguments! Usage: <infile> [-o <outfile>] [-is <arch>]"
    analyzeArgs (x:(y:xs))
        | x == "-o" = Map.insert "out" y (analyzeArgs xs)
        | x == "-is" = Map.insert "arch" y (analyzeArgs xs)
        | otherwise = error "Invalid arguments! Usage: <infile> [-o <outfile>] [-is <arch>]"

    extractArgs :: [String] -> Map.Map String String
    extractArgs [] = error "Invalid arguments! Usage: <infile> [-o <outfile>] [-is <arch>]"
    extractArgs (x:xs) = 
--      fill in default args
        if Map.notMember "out" m
            then Map.insert "out" "out.ll" (if Map.notMember "arch" m
                then Map.insert "arch" "---" m
                else m)
            else m
        where m = Map.insert "in" x (analyzeArgs xs)

    mainloop :: Handle -> Handle -> IO ()
    mainloop infile outfile = do 
        ineof <- hIsEOF infile
        if ineof then return ()
        else do 
            inpStr <- hGetLine infile
            hPutStrLn outfile inpStr
            mainloop infile outfile

    handleFileIO :: Map.Map String String -> IO()
    handleFileIO m = do
        input <- readFile (Map.findWithDefault "in.txt" "in" m)
        b <- writeFile (Map.findWithDefault "" "out" m) ""
        output <- openFile (Map.findWithDefault "" "out" m) WriteMode
        c <- hDuplicateTo output stderr
        d <- hDuplicateTo output stdout
        let functionList = Parser.myParse input in do
            let (globalVariable,returnValue) = Run.runFunction (Variable.NewVariable "main",0,[],functionList, Map.empty) in putStrLn ("return value:" ++ (show returnValue))
        hClose output

    compiler :: IO ()
    compiler = do
        a <- getArgs
        handleFileIO(extractArgs a)