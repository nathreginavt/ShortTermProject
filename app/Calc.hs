module Calc where

-- DSL
data Expr = Const Int
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

-- Function to calculate
eval :: Expr -> Int
eval (Const x)   = x
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

-- 
calc :: IO()
calc = do
    putStrLn "Welcome to KidsCalculator! "
    putStrLn "Write the first value: "
    x <- readLn :: IO Int
    putStrLn "Write the second value: "
    y <- readLn :: IO Int
    putStrLn "Select the option you want: "
    putStrLn "1- Addition"
    putStrLn "2- Subtration"
    putStrLn "3- Multiplication"
    putStrLn "4- Division"
    op <- readLn :: IO Int
    let expression = case op of
                        1 -> Add (Const x) (Const y)
                        2 -> Sub (Const x) (Const y)
                        3 -> Mul (Const x) (Const y)
                        4 -> Div (Const x) (Const y)
                        _ -> error "Invalid option"
    let result = eval expression
    putStrLn ("Result: " ++ show result)
        

{--*

--1+2
expressao1 :: Expr
expressao1 = Const 1 `Add` Const 2

--2² - 5 x 2 + 6
expressao2 :: Expr
expressao2 = (Const 2 `Mul` Const 2) `Sub` (Const 5 `Mul` Const 2) `Add` Const 6

-- Função para criar expressões de forma mais conveniente
infixl 6 <+>
(<+>) :: Expr -> Expr -> Expr
(<+>) = Add

infixl 6 <->
(<->) :: Expr -> Expr -> Expr
(<->) = Sub

infixl 7 <*>
(<*>) :: Expr -> Expr -> Expr
(<*>) = Mul

infixl 7 </>
(</>) :: Expr -> Expr -> Expr
(</>) = Div

*--}