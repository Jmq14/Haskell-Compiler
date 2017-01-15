module Qsort where
	import qualified Run as Run
	import qualified Parser as Parser
	import qualified Expr as Expr
	import qualified Variable as Variable

	import qualified Data.Map as Map
	import qualified Data.List.Split as Split
	import qualified Data.List as List

	get_len :: [a] -> Integer
	get_len (x:xs) = 1 + (get_len xs)
	get_len [] = 0

	convertToString :: Rational -> String
	convertToString x =
		let result = Split.splitOn " " (show x) ;
			v1 = read (head result) :: Float ;
			v2 = read (last result) :: Float in
			show (v1/v2)

	convertToInt :: Rational -> Integer
	convertToInt x =
		let result = Split.splitOn " " (show x) ;
			v1 = read (head result) :: Integer ;
			v2 = read (last result) :: Float in
			v1

	qsort_pre_code =
			"(define (get i) " ++
			"(return (vector-ref z i)) " ++
			") " ++
			"(define (qsort l r v i j) " ++
			"(begin " ++
			"(if (>= l r) " ++
			"(return 0) " ++
			"skip " ++
			") " ++
			"(set! v (get l)) " ++
			"(set! i l) " ++
			"(set! j r) " ++
			"(while (< i j) " ++
			"(begin " ++
			"(while (and (< i j) (<= v (get j))) " ++
			"(set! j (- j 1)) " ++
			") " ++
			"(vector-set! z i (get j)) " ++
			"(while (and (< i j) (>= v (get i))) " ++
			"(set! i (+ i 1)) " ++
			") " ++
			"(vector-set! z j (get i)) " ++
			") " ++
			") " ++
			"(vector-set! z i v) " ++
			"(set! gg (qsort (+ i 1) r 0 0 0)) " ++
			"(set! gg (qsort l (- i 1) 0 0 0)) " ++
			"(return 0) " ++
			") " ++
			") " ++
			"(define (main) " ++
			"(begin "

	gen_assign :: Integer -> [Integer] -> String
	gen_assign idx (x:xs) = "(vector-set! z " ++ (show idx) ++ " " ++ (show x) ++ ") " ++ (gen_assign (idx+1) xs)
	gen_assign _ [] = " "

	qsort_mid_code :: [Integer] -> String
	qsort_mid_code z =
		let len = get_len z in
			("(set! n " ++ (show len) ++ ") (make-vector z " ++ (show len) ++ ") " ++ (gen_assign 0 z) ++ " ")
	
	qsort_next_code = 
		"(set! gg (qsort 0 (- n 1) 0 0 0)) (return z) ) )"

	gen_qsort_code :: [Integer] -> String
	gen_qsort_code z = qsort_pre_code ++ (qsort_mid_code z) ++ qsort_next_code

	get_run_value :: [Integer] -> Expr.Constant
	get_run_value z =
		let (_,value) = Run.runFunction (Variable.NewVariable "main",0,[],Parser.myParse (gen_qsort_code z),Map.empty) in value

	get_rational :: Expr.Constant -> Integer
	get_rational (Expr.FloatConstant x) = convertToInt x
	get_rational _ = error "sb"

	map_to_list :: Integer -> Expr.Constant -> [Integer]
	map_to_list idx (Expr.ArrayConstant len z) =
		if (idx == len)
			then []
			else (get_rational (Map.findWithDefault Expr.ErrorConstant idx z)):(map_to_list (idx+1) (Expr.ArrayConstant len z))
	map_to_list _ _ = error "sb"
			
	prop_qsort :: [Integer] -> Bool
	prop_qsort zz = let z = (100:(take 100 zz)) in ((map_to_list 0 (get_run_value z)) == (List.sort z))

