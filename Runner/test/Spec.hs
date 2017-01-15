import qualified Lib as Lib

import qualified Proposition as Proposition

import qualified Control.Exception as Exception

import qualified Test.QuickCheck as QuickCheck

catchAny :: IO a -> (Exception.SomeException -> IO a) -> IO a
catchAny = Exception.catch

checklist = ["arr","fib","fib_arr","func1","func2","middle","qsort","qsort_big","queen","queen_fast","test1","test2","test3"]

testCode s = do
	code <- readFile ("test/test_file/" ++ s ++ "/code")
	answer <- readFile ("test/test_file/" ++ s ++ "/answer")
	Lib.normalMind code "value" ("test/test_file/" ++ s ++ "/output")
	output <- readFile ("test/test_file/" ++ s ++ "/output")
	if (output /= answer)
		then putStrLn "Error"
		else putStrLn "Passed"

testList [] = do
	putStrLn "Code test done."

testList (x:xs) =  do
	putStr (x ++ " : ")
	testCode x
	testList xs

propertyTest = do
	putStr "prop_add : " 
	QuickCheck.quickCheck Proposition.prop_add
	putStr "prop_qsort : " 
	QuickCheck.quickCheck Proposition.prop_qsort
	putStr "prop_queen : " 
	QuickCheck.quickCheckWith QuickCheck.stdArgs { QuickCheck.maxSuccess = 20 } Proposition.prop_queen

main :: IO ()
main = do
	putStrLn "\n"
	putStrLn "Test begin\n"
	putStrLn "Code test begin:"
	testList checklist
	putStrLn ""
	putStrLn "Property test begin:"
	propertyTest
	putStrLn "Property test done.\n"
	putStrLn "Test done"
