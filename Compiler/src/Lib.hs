module Lib where
	import System.Environment
	import System.Exit

	compiler :: IO ()
	compiler = do
		a <- getArgs
		print a