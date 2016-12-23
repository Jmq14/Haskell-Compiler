module Expr where
	
	data DataType = BoolType | FloatType | StringType | CharType | PairType DataType DataType | ErrorType deriving (Show, Eq);

	data OperatorType = NotOperator | AndOperator | OrOperator | PlusOperator | MinusOperator | MultiplicationOperator | DivisionOperator | EqualOperator | LessOperator | LeqOperator | GreatOperator | GeqOperator | ConsOperator | CarOperator | CdrOperator deriving (Show, Eq);

	data Constant = BoolConstant Bool | FloatConstant Float | StringConstant String | CharConstant Char | PairConstant (Constant,Constant) | ErrorConstant deriving (Show, Eq);

	data Expr = EmptyExpr | NewConstant Constant | NewExpr OperatorType DataType Expr Expr deriving (Show, Eq);

	getExprType :: Expr -> DataType
	getExprType EmptyExpr						= ErrorType
	getExprType (NewConstant (BoolConstant _))	= BoolType
	getExprType (NewConstant (FloatConstant _))	= FloatType
	getExprType (NewConstant ErrorConstant)		= ErrorType
	getExprType (NewExpr _ t _ _)				= t;

	getPairLeftType :: Expr -> DataType
	getPairLeftType (NewExpr _ (PairType l r) _ _) = l;
	getPairLeftType _ = ErrorType;
	
	getPairRightType :: Expr -> DataType
	getPairRightType (NewExpr _ (PairType l r) _ _) = r;
	getPairRightType _ = ErrorType;

	notConstant :: Constant -> Constant
	notConstant (BoolConstant x) = BoolConstant (not x)
	notConstant _ = ErrorConstant

	plusConstant :: Constant -> Constant -> Constant
	plusConstant (FloatConstant v1) (FloatConstant v2) = FloatConstant (v1+v2);
	plusConstant _ _ = ErrorConstant;

	minusConstant :: Constant -> Constant -> Constant
	minusConstant (FloatConstant v1) (FloatConstant v2) = FloatConstant (v1-v2);
	minusConstant _ _ = ErrorConstant;

	multiplicationConstant :: Constant -> Constant -> Constant
	multiplicationConstant (FloatConstant v1) (FloatConstant v2) = FloatConstant (v1*v2);
	multiplicationConstant _ _ = ErrorConstant;

	divisionConstant :: Constant -> Constant -> Constant
	divisionConstant (FloatConstant v1) (FloatConstant v2) = FloatConstant (v1/v2);
	divisionConstant _ _ = ErrorConstant;

	equalConstant :: Constant -> Constant -> Constant
	equalConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1==v2);
	equalConstant _ _ = ErrorConstant;

	lessConstant :: Constant -> Constant -> Constant
	lessConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1<v2);
	lessConstant _ _ = ErrorConstant;

	leqConstant :: Constant -> Constant -> Constant
	leqConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1<=v2);
	leqConstant _ _ = ErrorConstant;

	greatConstant :: Constant -> Constant -> Constant
	greatConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1>v2);
	greatConstant _ _ = ErrorConstant;

	geqConstant :: Constant -> Constant -> Constant
	geqConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1>=v2);
	geqConstant _ _ = ErrorConstant;

	consConstant :: Constant -> Constant -> Constant
	consConstant x y = PairConstant (x,y)

	carConstant :: Constant -> Constant
	carConstant (PairConstant (x,y)) = x
	carConstant _ = ErrorConstant;

	cdrConstant :: Constant -> Constant
	cdrConstant (PairConstant (x,y)) = y
	cdrConstant _ = ErrorConstant

	valueOfExpr :: Expr -> Constant
	valueOfExpr EmptyExpr = error "Something Wrong!";
	valueOfExpr (NewConstant (BoolConstant x)) = BoolConstant x;
	valueOfExpr (NewConstant (FloatConstant x)) = FloatConstant x;
	valueOfExpr (NewConstant ErrorConstant) = ErrorConstant;
	valueOfExpr (NewExpr operator datatype expr1 expr2)
		| operator == NotOperator	= notConstant (valueOfExpr expr1)
		| operator == OrOperator	= let leftValue = valueOfExpr expr1 in
			if ( leftValue == ErrorConstant || leftValue == (BoolConstant True))
				then leftValue
				else valueOfExpr expr2
		| operator == AndOperator	= let leftValue = valueOfExpr expr1 in
			if ( leftValue == ErrorConstant || leftValue == (BoolConstant False))
				then leftValue
				else valueOfExpr expr2
		| operator == PlusOperator = plusConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == MinusOperator = minusConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == MultiplicationOperator = multiplicationConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == DivisionOperator = divisionConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == EqualOperator = equalConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == LessOperator = lessConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == LeqOperator = leqConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == GreatOperator = greatConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == GeqOperator = geqConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == ConsOperator = consConstant (valueOfExpr expr1) (valueOfExpr expr2)
		| operator == CarOperator = carConstant (valueOfExpr expr1)
		| operator == CdrOperator = cdrConstant (valueOfExpr expr1)
