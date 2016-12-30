import os
import sys

os.system("stack build");

strs = ""
for a in range(1,len(sys.argv)):
	strs = strs + " " + sys.argv[a]

os.system("stack exec -- Haskell-Compiler-exe"+strs)

