module Expr where
	import qualified Variable as Variable;

	import qualified Data.Map as Map;
	import qualified Data.Ratio as Ratio;

	import qualified Debug.Trace as Trace
	
	data DataType = BoolType | FloatType | StringType | CharType | PairType DataType DataType | ArrayType | ErrorType deriving (Show, Eq);

	data OperatorType = NotOperator | AndOperator | OrOperator | PlusOperator | MinusOperator | MultiplicationOperator | DivisionOperator | EqualOperator | LessOperator | LeqOperator | GreatOperator | GeqOperator | ConsOperator | CarOperator | CdrOperator deriving (Show, Eq);

	data Constant = BoolConstant Bool | FloatConstant Rational | StringConstant String | CharConstant Char | PairConstant (Constant,Constant) | VariableConstant Variable.Variable | ArrayConstant Integer (Map.Map Integer Constant) | ErrorConstant deriving (Show, Eq);

	data Expr = EmptyExpr | NewConstant Constant | NewExpr OperatorType DataType Expr Expr | ArrayExpr Variable.Variable Expr | FunctionExpr Variable.Variable Integer [Expr] deriving (Show, Eq);

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

	valueOfArrayElement :: Constant -> Integer -> Constant
	valueOfArrayElement (ArrayConstant len z) idx =
		if (idx < len && idx >= 0)
			then Map.findWithDefault ErrorConstant idx z
			else error ("The index " ++ (show idx) ++ " is an avaliable index of range [0," ++ (show len) ++ ")")
	valueOfArrayElement _ _ = ErrorConstant

	getConstantType :: Constant -> DataType
	getConstantType (BoolConstant _)	= BoolType
	getConstantType (FloatConstant _)	= FloatType
	getConstantType (StringConstant _)	= StringType
	getConstantType (CharConstant _)	= CharType
	getConstantType _					= ErrorType

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

	checkWhetherBool :: Constant -> Bool
	checkWhetherBool (BoolConstant _) = True
	checkWhetherBool _ = False

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
