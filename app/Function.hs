module Function where
	import qualified Variable as Variable
	import qualified Tree as Tree
	import qualified Expr as Expr

	import qualified Data.Map as Map

	-- 定义函数类型
	data Function = ErrorFunction | NewFunction Variable.Variable Integer [Variable.Variable] Tree.Node deriving (Show, Eq);
