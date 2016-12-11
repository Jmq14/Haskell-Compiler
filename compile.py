import os
import platform

os.system('cabal install -j');

if (platform.system() == "Windows"):
	os.system("rmdir /S /Q bin");
	os.system("mkdir bin");
	os.system("copy .cabal-sandbox\\bin\\ bin\\ ");
else:
	os.system("rm bin -r");
	os.system("copy .cabal-sandbox/bin/ bin -r");
