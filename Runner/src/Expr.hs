module Expr where
	import qualified Variable as Variable;

	import qualified Data.Map as Map;
	import qualified Data.Ratio as Ratio;

	import qualified Debug.Trace as Trace

	-- ���п��ܵ���������
	data DataType = BoolType | FloatType | StringType | CharType | PairType DataType DataType | ArrayType | ErrorType deriving (Show, Eq);

	-- ���п��ܵ����������
	data OperatorType = NotOperator | AndOperator | OrOperator | PlusOperator | MinusOperator | MultiplicationOperator | DivisionOperator | EqualOperator | LessOperator | LeqOperator | GreatOperator | GeqOperator | ConsOperator | CarOperator | CdrOperator deriving (Show, Eq);

	-- ���п��ܵĳ�������
	data Constant = BoolConstant Bool | FloatConstant Rational | StringConstant String | CharConstant Char | PairConstant (Constant,Constant) | VariableConstant Variable.Variable | ArrayConstant Integer (Map.Map Integer Constant) | ErrorConstant deriving (Show, Eq);

	-- ���п��ܵı��ʽ����
	data Expr = EmptyExpr | NewConstant Constant | NewExpr OperatorType DataType Expr Expr | ArrayExpr Variable.Variable Expr | FunctionExpr Variable.Variable Integer [Expr] deriving (Show, Eq);

	-- ��������ʾ���������ڽ��������е����
	notPrettyShow :: Constant -> String
	notPrettyShow (BoolConstant x) = show x
	notPrettyShow (FloatConstant x) = show x 
	notPrettyShow (StringConstant x) = "''" ++ x ++ "''"
	notPrettyShow (CharConstant x) = "'" ++ (x:[]) ++ "'"
	notPrettyShow (PairConstant (l,r)) = "(" ++ (notPrettyShow l) ++ "," ++ (notPrettyShow r) ++ ")"
	notPrettyShow (ArrayConstant len x) = "[" ++ (anotherShow 0 len x) ++ "]"
	notPrettyShow ErrorConstant = "undefined"

	-- ����������������ʾ���鳣��
	anotherShow :: Integer -> Integer -> Map.Map Integer Constant -> String
	anotherShow idx len x =
		if (idx == len)
			then ""
			else
				let value = Map.findWithDefault ErrorConstant idx x in
					if (idx == 0)
						then (notPrettyShow value) ++ (anotherShow (idx+1) len x)
						else "," ++ (notPrettyShow value) ++ (anotherShow (idx+1) len x)

	-- ��鳣���Ƿ�Ϊ����
	checkConstantWhetherInt :: Constant -> Bool
	checkConstantWhetherInt (FloatConstant f) = (Ratio.denominator f) == 1
	checkConstantWhetherInt _ = False

	-- ������ת��Ϊ����
	convertConstantToInteger :: Constant -> Integer
	convertConstantToInteger (FloatConstant f) = Ratio.numerator f

	-- ��鳣���Ƿ�Ϊ����
	checkConstantWhetherArray :: Constant -> Bool
	checkConstantWhetherArray (ArrayConstant _ _) = True
	checkConstantWhetherArray _ = False

	-- �������鳣����ĳ��Ԫ��
	visitArrayValue :: Constant -> Integer -> Constant
	visitArrayValue (ArrayConstant len z) idx =
		if (idx < len && idx >= 0)
			then let arrValue = Map.findWithDefault ErrorConstant idx z in
				if (arrValue == ErrorConstant)
					then error "Runtime Error: vector element hasn't been defined before."
					else arrValue
			else error ("Runtime Error: the index " ++ (show idx) ++ " is an avaliable index of range [0," ++ (show len) ++ ")")
	visitArrayValue c id = error ("Runtime error: " ++ (notPrettyShow c) ++ "is not an array")

	-- ��������Ԫ�ص�ֵ
	updateArrayValue :: Constant -> Integer -> Constant -> Constant
	updateArrayValue (ArrayConstant len mem) idx val =
		if (idx>=0 && idx<len)
			then (ArrayConstant len (Map.insert idx val mem))
			else ErrorConstant
	updateArrayValue _ _ _ = error "Runtime Error: vector-set! must work on array element."

	-- ���Ժ�������ȡ��������
	getConstantType :: Constant -> DataType
	getConstantType (BoolConstant _)	= BoolType
	getConstantType (FloatConstant _)	= FloatType
	getConstantType (StringConstant _)	= StringType
	getConstantType (CharConstant _)	= CharType
	getConstantType _					= error ("Debug Error: Why you call this?(getConstantType)")

	-- ���Ժ�������ȡ���ʽ����
	getExprType :: Expr -> DataType
	getExprType EmptyExpr						= error ("Debug Error: Why you call this?(getExprType)")
	getExprType (NewConstant (BoolConstant _))	= BoolType
	getExprType (NewConstant (FloatConstant _))	= FloatType
	getExprType (NewConstant ErrorConstant)		= ErrorType
	getExprType (NewExpr _ t _ _)				= t;

	-- ���Ժ�������ȡPair�������Ԫ������
	getPairLeftType :: Expr -> DataType
	getPairLeftType (NewExpr _ (PairType l r) _ _) = l;
	getPairLeftType _ = ErrorType;
	
	-- ���Ժ�������ȡPair�����Ҳ�Ԫ������
	getPairRightType :: Expr -> DataType
	getPairRightType (NewExpr _ (PairType l r) _ _) = r;
	getPairRightType _ = ErrorType;

	-- ��鳣���Ƿ�ΪBool����
	checkWhetherBool :: Constant -> Bool
	checkWhetherBool (BoolConstant _) = True
	checkWhetherBool _ = False

	-- not����
	notConstant :: Constant -> Constant
	notConstant (BoolConstant x) = BoolConstant (not x)
	notConstant _ = ErrorConstant

	-- �ӷ�����
	plusConstant :: Constant -> Constant -> Constant
	plusConstant (FloatConstant v1) (FloatConstant v2) = FloatConstant (v1+v2);
	plusConstant v1 v2 = error ("Runtime error: " ++ (notPrettyShow v1) ++ " + " ++ (notPrettyShow v2) ++ " is not an available expression");

	-- ��������
	minusConstant :: Constant -> Constant -> Constant
	minusConstant (FloatConstant v1) (FloatConstant v2) = FloatConstant (v1-v2);
	minusConstant v1 v2 = error ("Runtime error: " ++ (notPrettyShow v1) ++ " + " ++ (notPrettyShow v2) ++ " is not an available expression")

	-- �˷�����
	multiplicationConstant :: Constant -> Constant -> Constant
	multiplicationConstant (FloatConstant v1) (FloatConstant v2) = FloatConstant (v1*v2);
	multiplicationConstant v1 v2 = error ("Runtime error: " ++ (notPrettyShow v1) ++ " * " ++ (notPrettyShow v2) ++ " is not an available expression");

	-- ��������
	divisionConstant :: Constant -> Constant -> Constant
	divisionConstant (FloatConstant v1) (FloatConstant v2) = FloatConstant (v1/v2);
	divisionConstant v1 v2 = error ("Runtime error: " ++ (notPrettyShow v1) ++ " / " ++ (notPrettyShow v2) ++ " is not an available expression");

	-- �������
	equalConstant :: Constant -> Constant -> Constant
	equalConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1==v2);
	equalConstant (BoolConstant v1) (BoolConstant v2) = BoolConstant (v1==v2);
	equalConstant (CharConstant v1) (CharConstant v2) = BoolConstant (v1==v2);
	equalConstant v1 v2 = error ("Runtime error: " ++ (notPrettyShow v1) ++ " = " ++ (notPrettyShow v2) ++ " is not an available expression");

	-- С������
	lessConstant :: Constant -> Constant -> Constant
	lessConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1<v2);
	lessConstant v1 v2 = error ("Runtime error: " ++ (notPrettyShow v1) ++ " < " ++ (notPrettyShow v2) ++ " is not an available expression");

	-- С�ڵ�������
	leqConstant :: Constant -> Constant -> Constant
	leqConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1<=v2);
	leqConstant v1 v2 = error ("Runtime error: " ++ (notPrettyShow v1) ++ " <= " ++ (notPrettyShow v2) ++ " is not an available expression");

	-- ��������
	greatConstant :: Constant -> Constant -> Constant
	greatConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1>v2);
	greatConstant v1 v2 = error ("Runtime error: " ++ (notPrettyShow v1) ++ " > " ++ (notPrettyShow v2) ++ " is not an available expression");

	-- ���ڵ�������
	geqConstant :: Constant -> Constant -> Constant
	geqConstant (FloatConstant v1) (FloatConstant v2) = BoolConstant (v1>=v2);
	geqConstant v1 v2 = error ("Runtime error: " ++ (notPrettyShow v1) ++ " >= " ++ (notPrettyShow v2) ++ " is not an available expression")

	-- cons����
	consConstant :: Constant -> Constant -> Constant
	consConstant x y = PairConstant (x,y)

	-- car����
	carConstant :: Constant -> Constant
	carConstant (PairConstant (x,y)) = x
	carConstant v = error ("Runtime error: car operator is not available on " ++ (notPrettyShow v));

	 -- cdr����
	cdrConstant :: Constant -> Constant
	cdrConstant (PairConstant (x,y)) = y
	cdrConstant v = error ("Runtime error: cdr operator is not available on " ++ (notPrettyShow v));
