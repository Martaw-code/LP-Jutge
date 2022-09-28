data Expr = 
    Val Int | 
    Add Expr Expr | 
    Sub Expr Expr | 
    Mul Expr Expr | 
    Div Expr Expr
    deriving (Show)

eval1 :: Expr -> Int
eval1 (Val x) = x
eval1 (Add x y) = (eval1 x) + (eval1 y)
eval1 (Sub x y) = (eval1 x) - (eval1 y)
eval1 (Mul x y) = (eval1 x) * (eval1 y)
eval1 (Div x y) = div (eval1 x) (eval1 y)


eval2 :: Expr -> Maybe Int
eval2 (Val x) = Just x
eval2 (Add x y) = do
    ex <- (eval2 x)
    ey <- (eval2 y)
    Just (ex+ey)
eval2 (Sub x y) = do
    ex <- (eval2 x)
    ey <- (eval2 y)
    Just (ex-ey)
eval2 (Mul x y) = do
    ex <- (eval2 x)
    ey <- (eval2 y)
    Just (ex*ey)
eval2 (Div x y) = do
    ex <- (eval2 x)
    ey <- (eval2 y)
    if ey == 0 then Nothing else Just(div ex ey)

eval3 :: Expr -> Either String Int
eval3 (Val x) = Right x
eval3 (Add x y) = do
    ex <- (eval3 x)
    ey <- (eval3 y)
    Right(ex+ey)
eval3 (Sub x y) = do
    ex <- (eval3 x)
    ey <- (eval3 y)
    Right(ex-ey)
eval3 (Mul x y) = do
    ex <- (eval3 x)
    ey <- (eval3 y)
    Right(ex*ey)
eval3 (Div x y) = do
    ex <- (eval3 x)
    ey <- (eval3 y)
    if ey == 0 then Left "div0" else Right(div ex ey)
    
