module Expr where
	import qualified Variable as Variable;

	import qualified Data.Map as Map;
	import qualified Data.Ratio as Ratio;
	
	data DataType = BoolType | FloatType | StringType | CharType | PairType DataType DataType | ArrayType | ErrorType deriving (Show, Eq);

	data OperatorType = NotOperator | AndOperator | OrOperator | PlusOperator | MinusOperator | MultiplicationOperator | DivisionOperator | EqualOperator | LessOperator | LeqOperator | GreatOperator | GeqOperator | ConsOperator | CarOperator | CdrOperator deriving (Show, Eq);

	data Constant = BoolConstant Bool | FloatConstant Rational | StringConstant String | CharConstant Char | PairConstant (Constant,Constant) | VariableConstant Variable.Variable | ArrayConstant Integer (Map.Map Integer Constant) | ErrorConstant deriving (Show, Eq);

	data Expr = EmptyExpr | NewConstant Constant | NewExpr OperatorType DataType Expr Expr | ArrayExpr Variable.Variable Expr deriving (Show, Eq);

	checkConstantWhetherInt :: Constant -> Bool
	checkConstantWhetherInt (FloatConstant f) = (Ratio.denominator f) == 1

	convertConstantToInteger :: Constant -> Integer
	convertConstantToInteger (FloatConstant f) = Ratio.numerator f

	checkConstantWhetherArray :: Constant -> Bool
	checkConstantWhetherArray (ArrayConstant _ _) = True
	checkConstantWhetherArray _ = False

	visitArrayValue :: Constant -> Integer -> Constant
	visitArrayValue (ArrayConstant len mem) idx =
		if (idx >= 0 && idx < len)
			then Map.findWithDefault ErrorConstant idx mem
			else ErrorConstant

	updateArrayValue :: Constant -> Integer -> Constant -> Constant
	updateArrayValue (ArrayConstant len mem) idx val =
		if (idx>=0 && idx<len)
			then (ArrayConstant len (Map.insert idx val mem))
			else ErrorConstant
	updateArrayValue _ _ _ = ErrorConstant

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

	valueOfArrayElement :: Constant -> Integer -> Constant
	valueOfArrayElement (ArrayConstant len z) idx =
		if (idx < len && idx >= 0)
			then Map.findWithDefault ErrorConstant idx z
			else ErrorConstant
	valueOfArrayElement _ _ = ErrorConstant

	valueOfExpr :: Expr -> Map.Map Variable.Variable Constant -> Constant
	valueOfExpr EmptyExpr _= error "Something Wrong!";
	valueOfExpr (NewConstant (BoolConstant x)) _ = BoolConstant x;
	valueOfExpr (NewConstant (FloatConstant x)) _ = FloatConstant x;
	valueOfExpr (NewConstant (CharConstant x)) _ = CharConstant x;
	valueOfExpr (NewConstant (StringConstant x)) _ = StringConstant x;
	valueOfExpr (NewConstant ErrorConstant) _ = ErrorConstant;
	valueOfExpr (NewConstant (VariableConstant nowvar)) variable =
		Map.findWithDefault ErrorConstant nowvar variable;
	valueOfExpr (NewExpr operator datatype expr1 expr2) variable
		| operator == NotOperator	= notConstant (valueOfExpr expr1 variable)
		| operator == OrOperator	= let leftValue = valueOfExpr expr1 variable in
			if ( leftValue == ErrorConstant || leftValue == (BoolConstant True))
				then leftValue
				else valueOfExpr expr2 variable
		| operator == AndOperator	= let leftValue = valueOfExpr expr1 variable in
			if ( leftValue == ErrorConstant || leftValue == (BoolConstant False))
				then leftValue
				else valueOfExpr expr2 variable
		| operator == PlusOperator = plusConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == MinusOperator = minusConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == MultiplicationOperator = multiplicationConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == DivisionOperator = divisionConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == EqualOperator = equalConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == LessOperator = lessConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == LeqOperator = leqConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == GreatOperator = greatConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == GeqOperator = geqConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == ConsOperator = consConstant (valueOfExpr expr1 variable) (valueOfExpr expr2 variable)
		| operator == CarOperator = carConstant (valueOfExpr expr1 variable)
		| operator == CdrOperator = cdrConstant (valueOfExpr expr1 variable)
	
	valueOfExpr (ArrayExpr var expr) variable =
		let value = valueOfExpr expr variable ; nowvar = Map.findWithDefault ErrorConstant var variable in 
			if ((checkConstantWhetherInt value) && (checkConstantWhetherArray nowvar))
				then visitArrayValue nowvar (convertConstantToInteger value)
				else ErrorConstant 
