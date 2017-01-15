import qualified KeyWord as KeyWord

checklist = ["arr","fib","fib_arr","func1","fubc2","middle","qsort","qsort_big","queen","queen_fast","test1","test2","test3","test4"]

testList [] = do
	putStrLn (show KeyWord.keywords)
	putStrLn "Code test done."

testList (x:xs) =  do
	putStrLn x
	testList xs

main :: IO ()
main = do
	putStrLn "\n"
	putStrLn "Test begin\n"
	putStrLn "Code test begin:"
	testList checklist
	putStrLn ""
	putStrLn "Property test begin:"
	putStrLn "Property test done.\n"
	putStrLn "Test done"
