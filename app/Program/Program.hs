module Program.Program(solutions, solutions', solutions'') where 

import Program.Operations
import Data.List (subsequences, permutations, delete, minimum, sortBy)

{- THE COUNTDOWN SOLVER -}

-- return all subsequences of a list
subs :: [a] -> [[a]]
-- subs = subsequences
subs [] = [[]]
subs (x:xs) = yss ++ map (x:) yss
    where yss = subs xs

-- returns all possible ways of inserting a new element into a list
interleave :: a -> [a] -> [[a]]
-- avoid using ++ and try use pattern matching where possible!
-- interleave x xs = [ l ++ [x] ++ r | n <- [0..length xs]
--                                 , let (l, r) = splitAt n xs ]
interleave x [] = [[x]]
interleave x (y:ys) = (x:y:ys) : map (y:) (interleave x ys)

perms :: [a] -> [[a]]
-- perms = permutations
perms [] = [[]]
perms (x:xs) = concatMap (interleave x) (perms xs)

-- return all choices in a list that the user can make from a list of cards
choices :: [a] -> [[a]]
-- choices xs = concatMap perms (subs xs)
choices xs = [ cs | ss <- subs xs 
                  , cs <- perms ss]

isChoice :: [Int] -> [Int] -> Bool 
isChoice [] ns = True 
isChoice (x:xs) ns 
    | x `elem` ns = isChoice xs (delete x ns)
    | otherwise = False

-- return whether or not an expression is a correct solution of the problem
solution :: Expr -> [Int] -> Int -> Bool 
solution e ns n = isChoice (values e) ns && eval e == [n]

{- BRUTE FORCE SOLUTION -}

-- return all possible ways of splitting a list into 2 non empty lists that append to give the original list
split :: [a] -> [([a], [a])]
split xs = [splitAt n xs | n <- [1..(length xs - 1)]]

-- all possible operations
ops :: [Op] 
ops = [Add, Sub, Mul, Div]

-- create all possible expressions with 2 expressions
combine :: Expr -> Expr -> [Expr] 
combine l r = [App o l r | o <- ops]

-- retun all possible expressions whose list of values is precisely a given list
exprs :: [Int] -> [Expr] 
exprs [] = []
exprs [x] = [Val x]
exprs xs = [ e | (ls, rs) <- split xs
               , l <- exprs ls 
               , r <- exprs rs
               , e <- combine l r ]

-- returns all possible expressions that solve an instance of the countdown problem 
-- first generate all expressions over each choice from the given list of numbers
-- then selecting those expressions that successfully evaluate to give the target
solutions :: [Int] -> Int -> [Expr]
solutions ns n = [ e | ns' <- choices ns
                     , e <- exprs ns'
                     , eval e == [n] ]

{- INCREASING EFFICIENCY -}

type Result = (Expr, Int)

combine' :: Result -> Result -> [Result]
combine' (l, x) (r, y) = [(App o l r, apply o x y) | o <- ops
                                                   , valid o x y ]

-- returns all possible results comprising expresions whose list of values is prcisely a given list
results :: [Int] -> [Result]
results [] = []
results [x] = [(Val x, x) | x > 0]
results xs = [ r | (ls, rs) <- split xs
                      , l <- results ls 
                      , r <- results rs 
                      , r <- combine' l r ]

-- calculate distance between values
diff :: Int -> Int -> Int 
diff x y = abs (x - y)

solutions' :: [Int] -> Int -> [Expr] 
solutions' ns n = [ e | ns' <- choices ns 
                      , (e, v) <- results ns'
                      , v == n ]

simpScore :: Expr -> Int 
simpScore (Val _) = 1
simpScore (App o l r) = 1 + simpScore l + simpScore r

sortBySimp :: [Expr] -> [Expr]
sortBySimp = sortBy (\l r -> compare (simpScore l) (simpScore r)) 

-- return nearest solutions if no solutions
solutions'' :: [Int] -> Int -> [Expr] 
solutions'' ns n = case s of 
    [] -> 
        let all = [ (e, diff v n) | ns' <- choices ns 
                                  , (e, v) <- results ns' ]
            min = minimum (map snd all)
        in sortBySimp [ e | (e, d) <- all 
                          , d == min ]
    _ -> sortBySimp s
    where s = solutions' ns n

{-
BENCHMARKING RESULTS BEFORE ALTERING 'VALID' FUNCTION:
Solutions
    1st: OK
      7.80 μs ± 728 ns
    2nd: OK
      3.31 μs ± 218 ns

RESULTS AFTER:
Solutions
    1st: OK
      8.56 μs ± 732 ns
    2nd: OK
      2.34 μs ±  68 ns
-}

