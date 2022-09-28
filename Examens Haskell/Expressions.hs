data Expr = Val Int | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = (eval1 x) + (eval1 y)
eval1 (Sub x y) = (eval1 x) - (eval1 y)
eval1 (Mul x y) = (eval1 x) * (eval1 y)
eval1 (Div x y) = div (eval1 x) (eval1 y)


eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just x
eval2 (Add x y) = do
    ev1 <- (eval2 x)
    ev2 <- (eval2 y)
    Just(ev1+ev2) 
eval2 (Sub x y) = do
    ev1 <- (eval2 x)
    ev2 <- (eval2 y)
    Just(ev1-ev2) 
eval2 (Mul x y) = do
    ev1 <- (eval2 x)
    ev2 <- (eval2 y)
    Just(ev1*ev2) 
eval2 (Div x y) = do
    ev1 <- (eval2 x)
    ev2 <- (eval2 y)
    if ev2 == 0 then Nothing else Just(div ev1 ev2)
    

eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add x y) = do
    ev1 <- (eval3 x)
    ev2 <- (eval3 y)
    Right(ev1+ev2)
eval3 (Sub x y) = do
    ev1 <- (eval3 x)
    ev2 <- (eval3 y)
    Right(ev1-ev2)
eval3 (Mul x y) = do
    ev1 <- (eval3 x)
    ev2 <- (eval3 y)
    Right(ev1*ev2)
eval3 (Div x y) = do
    ev1 <- (eval3 x)
    ev2 <- (eval3 y)
    if ev2 == 0 then Left "div0" else Right(div ev1 ev2)
