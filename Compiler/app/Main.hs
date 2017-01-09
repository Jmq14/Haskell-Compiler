module Main where
	import Lib
	
	main :: IO ()
	main = do
		a <- getArgs
		print a
		mainWork (analyzeArgs a)
