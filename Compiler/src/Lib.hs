module Lib where
    import System.Environment
    import System.Exit
    import System.IO
    import System.Process
    import GHC.IO.Handle

    import qualified Parser as Parser
    import qualified Expr as Expr
    import qualified Tree as Tree
    import qualified Variable as Variable
    import qualified ParseExpr as ParseExpr
    import qualified Translator as Translator

    import qualified Data.Map as Map
    import qualified Data.Time as Time

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
            then Map.insert "out" "out.py" (if Map.notMember "arch" m
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
        b <- writeFile (Map.findWithDefault "" "out" m ++".py") ""
        output <- openFile (Map.findWithDefault "" "out" m++".py") WriteMode
        c <- hDuplicateTo output stderr
        d <- hDuplicateTo output stdout
        let functionList = Parser.myParse input in do
            putStrLn (Translator.translate2python functionList)
--        r <- createProcess (proc "pyinstaller" []) { cmdspec = (Map.findWithDefault "" "out" m) []}
--        runProcess "pyinstaller" [(Map.findWithDefault "" "out" m)++".py", "-o", (Map.findWithDefault "" "out" m)] (Just "./") Nothing Nothing Nothing Nothing
--        run <- system ("pyinstaller "++Map.findWithDefault "" "out" m++".py -o "++Map.findWithDefault "" "out" m)
        (_,mOut,mErr,procHandle) <- createProcess $ (proc "pyinstaller" [(Map.findWithDefault "" "out" m)++".py"]) {std_out = CreatePipe, std_err = CreatePipe }
        hClose output

    compiler :: IO ()
    compiler = do
        start <- Time.getCurrentTime
        a <- getArgs
        handleFileIO(extractArgs a)
        end <- Time.getCurrentTime
        print (Time.diffUTCTime end start)
