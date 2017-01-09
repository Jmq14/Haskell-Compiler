module Function where
	import qualified Variable as Variable
	import qualified Tree as Tree
	import qualified Expr as Expr

	import qualified Data.Map as Map

	data Function = ErrorFunction | NewFunction Variable.Variable Integer [Variable.Variable] Tree.Node deriving (Show, Eq);
