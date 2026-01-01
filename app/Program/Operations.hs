module Program.Operations where 

{- OPERATIONS -}

data Op = Add | Mul | Sub | Div | Exp 
data Expr = Val Int | App Op Expr Expr

instance Show Op where 
    show Add = "+"
    show Mul = "*"
    show Sub = "-"
    show Div = "/"
    show Exp = "^"

instance Show Expr where 
    show (Val x) = show x 
    show (App o l r) = "(" ++ show l ++ show o ++ show r ++ ")"

-- decide if the application of an operator of 2 positive naturals gives another positive natural
valid :: Op -> Int -> Int -> Bool 
valid Add x y = x <= y 
valid Sub x y = x > y 
valid Mul x y = x /= 1 && y /= 1 && x <= y
valid Div x y = y /= 1 && x `mod` y == 0
valid Exp x y = y /= 1 && x /= 1

-- apply the operation to 2 positive naturals
apply :: Op -> Int -> Int -> Int 
apply Add x y = x + y 
apply Sub x y = x - y 
apply Mul x y = x * y 
apply Div x y = x `div` y 

-- return a list of values in an expression 
values :: Expr -> [Int] 
values (Val x) = [x]
values (App _ x y) = values x ++ values y 

-- evaluates the overall value of an expression if it is valid
-- if list returns singleton, success, otherwise failure
-- failure can also be done using Maybe, but list comprehension is a clean implementation of this
eval :: Expr -> [Int] 
eval (Val x) = [x | x > 0]
eval (App o x y) = [apply o l r | l <- eval x
                                , r <- eval y 
                                , valid o l r ]