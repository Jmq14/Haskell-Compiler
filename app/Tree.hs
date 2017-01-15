module Tree where
	import qualified Expr as Expr;
	import qualified Variable as Variable;

	-- ÉùÃ÷Óï¾ä½Úµã
	data Node = Nil | StatementListNode Node Node | SetVariableNode Variable.Variable Expr.Expr | WhileNode Expr.Expr Node | IfNode Expr.Expr Node Node | PrintNode Expr.Expr | MakeVectorNode Variable.Variable Expr.Expr | VectorSetNode Variable.Variable Expr.Expr Expr.Expr | ReturnNode Expr.Expr | ErrorNode deriving (Show, Eq);
