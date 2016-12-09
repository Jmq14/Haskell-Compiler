module Expr where
	
	data DataType = BoolType | FloatType | StringType | ErrorType deriving (Show, Eq);

	data OperatorType = NotOperator | AndOperator | OrOperator deriving (Show, Eq);

	data Expr = EmptyExpr | BoolConstant Bool | NewExpr OperatorType DataType Expr Expr deriving (Show, Eq);

	getExprType :: Expr -> DataType
	getExprType EmptyExpr			= ErrorType
	getExprType (BoolConstant _)	= BoolType
	getExprType (NewExpr _ t _ _)	= t;

	valueOfExpr EmptyExpr = error "Something Wrong!";
	valueOfExpr (BoolConstant x) = x;
	valueOfExpr (NewExpr operator datatype expr1 expr2)
		| operator == NotOperator	= not (valueOfExpr expr1)
		| operator == OrOperator	= 
			if ( (valueOfExpr expr1) == True)
				then True
				else valueOfExpr expr2
		| operator == AndOperator	=
			if ( (valueOfExpr expr1) == False)
				then False
				else valueOfExpr expr2						
