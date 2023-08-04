module Calc where

-- DSL
data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Exp Expr Expr

-- Function to calculate
calc :: Expr -> Int
calc (Const x)   = x
calc (Add e1 e2) = calc e1 + calc e2
calc (Sub e1 e2) = calc e1 - calc e2
calc (Mul e1 e2) = calc e1 * calc e2
calc (Div e1 e2) = calc e1 `div` calc e2
calc (Exp e1 e2) = calc e1 ^ calc e2
_ = error

--Kids Calculator
kidsCalc :: IO()
kidsCalc = do
    putStrLn ""
    putStrLn "Welcome to KidsCalculator! "
    putStrLn ""
    putStrLn "Write the first value: "
    x <- readLn :: IO Int
    putStrLn "Write the second value: "
    y <- readLn :: IO Int
    putStrLn "Select the option you want: "
    putStrLn ("1- Addition       (" ++ show x ++ "+" ++ show y ++ ")")
    putStrLn ("2- Subtraction    (" ++ show x ++ "-" ++ show y ++ ")")
    putStrLn ("3- Multiplication (" ++ show x ++ "x" ++ show y ++ ")")
    putStrLn ("4- Division       (" ++ show x ++ "/" ++ show y ++ ")")
    putStrLn ("5- Exponentiation (" ++ show x ++ "^" ++ show y ++ ")")
    op <- readLn :: IO Int
    let expression = case op of
                        1 -> Add (Const x) (Const y)
                        2 -> Sub (Const x) (Const y)
                        3 -> Mul (Const x) (Const y)
                        4 -> Div (Const x) (Const y)
                        5 -> Exp (Const x) (Const y)
                        _ -> error "Invalid option"
    let result = calc expression
    putStrLn ("Result: " ++ show result)
        

--Math expressions:

--1+2
expression1 :: Expr
expression1 = Const 1 `Add` Const 2

--2² - 5 x 2 + 6
expression2 :: Expr
expression2 = Const 2 `Exp` Const 2 `Sub` (Const 5 `Mul` Const 2) `Add` Const 6

-- (3 + 2) x (4 - 1)²
expression3 :: Expr
expression3 = (Const 3 `Add` Const 2) `Mul` ((Const 4 `Sub` Const 1) `Exp` Const 2)

-- -(3 + 5) x -2
expression4 :: Expr
expression4 = Mul (Sub (Const 0) (Add (Const 3) (Const 5))) (Sub (Const 0) (Const 2))

