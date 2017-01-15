module Proposition where
	import qualified Run as Run
	import qualified Parser as Parser
	import qualified Expr as Expr
	import qualified Variable as Variable

	import qualified Data.Map as Map

	import qualified Qsort as Qsort
	import qualified Queen as Queen

	prop_add :: Rational -> Rational -> Bool
	prop_add a b = let (c,d) = (Run.valueOfExpr (Expr.NewExpr Expr.PlusOperator Expr.FloatType (Expr.NewConstant (Expr.FloatConstant a)) (Expr.NewConstant (Expr.FloatConstant b))) Map.empty Map.empty Map.empty) in c == (Expr.FloatConstant (a+b))

			
	prop_qsort :: [Integer] -> Bool
	prop_qsort z = Qsort.prop_qsort z

	prop_queen n = Queen.prop_queen (((n `mod` 9) + 100) `mod` 9 + 1)

