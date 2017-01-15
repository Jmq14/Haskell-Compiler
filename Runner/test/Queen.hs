module Queen where
	import qualified Run as Run
	import qualified Parser as Parser
	import qualified Expr as Expr
	import qualified Variable as Variable

	import qualified Data.Map as Map
	import qualified Data.List.Split as Split
	
	import Debug.Trace

	convertToInt :: Rational -> Int
	convertToInt x =
		let result = Split.splitOn " " (show x) ;
			v1 = read (head result) :: Int ;
			v2 = read (last result) :: Float in
			v1

	pre_code =
		"(define (check i j) " ++
		"(begin " ++
		"(if (vector-ref col j) " ++
		"(return False) " ++
		"skip " ++
		")  " ++
		"(if (vector-ref right (+ i j)) " ++
		"(return False) " ++
		"skip " ++
		") " ++
		"(if (vector-ref left (+ (- i j) n)) " ++
		"(return False) " ++
		"skip " ++
		") " ++
		"(return True) " ++
		") " ++
		") " ++
		"(define (dfs i j) " ++
		"(begin " ++
		"(if (>= i n) " ++
		"(begin " ++
		"(set! ans (+ ans 1)) " ++
		"(return 0)  " ++
		") " ++
		"skip " ++
		") " ++
		"(while (< j n) " ++
		"(begin " ++
		"(if (check i j) " ++
		"(begin " ++
		"(vector-set! col j True) " ++
		"(vector-set! left (+ (- i j) n) True) " ++
		"(vector-set! right (+ i j) True) " ++
		"(set! void (dfs (+ i 1) 0)) " ++
		"(vector-set! col j False) " ++
		"(vector-set! left (+ (- i j) n) False) " ++
		"(vector-set! right (+ i j) False) " ++
		"(set! j (+ j 1)) " ++
		") " ++
		"(set! j (+ j 1)) " ++
		") " ++
		") " ++
		") " ++
		"(return 0) " ++
		") " ++
		") " ++
		"(define (main) " ++
		"(begin " ++
		"(set! ans 0) " 

	mid_code :: Int -> String
	mid_code n = 
		"(set! n " ++ (show n) ++ ") "
	
	next_code = 
		"(set! m (+ n (+ n 1))) " ++
		"(make-vector col m) " ++
		"(make-vector left m) " ++
		"(make-vector right m) " ++
		"(set! i 0) " ++
		"(while (< i m) " ++
		"(begin " ++
		"(vector-set! col i False) " ++
		"(vector-set! left i False) " ++
		"(vector-set! right i False) " ++
		"(set! i (+ i 1)) " ++
		") " ++
		") " ++
		"(set! void (dfs 0 0)) " ++
		"(return ans) " ++
		") " ++
		") "
	
	gen_code :: Int -> String
	gen_code n = pre_code ++ (mid_code n) ++ next_code

	get_rational :: Expr.Constant -> Int
	get_rational (Expr.FloatConstant x) = convertToInt x
	get_rational _ = error "sb"

	runCode :: Int -> Int
	runCode n =
		let (_,value) = Run.runFunction (Variable.NewVariable "main",0,[],Parser.myParse (gen_code n),Map.empty) in (get_rational value)

	value = [0,1,0,0,2,10,4,40,92,352,724,2680,14200,73712]

	prop_queen :: Int -> Bool
	prop_queen n = ((value !! n) == (runCode n))
