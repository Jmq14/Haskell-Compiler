import os
import sys

if (os.system("stack build")!=0):
	fsdfasdfdasafsd

strs = ""
for a in range(1,len(sys.argv)):
	strs = strs + " " + sys.argv[a]

os.system("stack exec -- Compiler-exe"+strs)

