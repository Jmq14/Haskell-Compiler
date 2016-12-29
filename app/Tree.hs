module Tree where
	import qualified Expr as Expr;
	import qualified Variable as Variable;
	
	data Node = Nil | StatementListNode Node Node | SetVariableNode Variable.Variable Expr.Expr | WhileNode Expr.Expr Node | IfNode Expr.Expr Node Node | ErrorNode deriving (Show, Eq);
