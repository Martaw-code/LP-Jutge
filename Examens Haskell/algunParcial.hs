-- Problema 1: Expressió postfixa 1

eval1 :: String -> Int
eval1 s = head $ eval1' (words s) []
    where
        eval1' :: [String] -> [Int] -> [Int]
        eval1' ("+":l) (x:y:pila) = eval1' l ((y+x):pila)
        eval1' ("-":l) (x:y:pila) = eval1' l ((y-x):pila)
        eval1' ("*":l) (x:y:pila) = eval1' l ((y*x):pila)
        eval1' ("/":l) (x:y:pila) = eval1' l ((div y x):pila)
        eval1' (x:l) pila = eval1' l ((read x):pila)
        eval1' [] pila = pila

-- Problema 2: Expressió postfixa 2

eval2 :: String -> Int
eval2 s = head $ foldl manage [] $ words s
    where
        manage :: [Int] -> String -> [Int]
        manage (x:y:l) "+" = ((y+x):l)
        manage (x:y:l) "-" = ((y-x):l)
        manage (x:y:l) "*" = ((y*x):l)
        manage (x:y:l) "/" = ((div y x):l)
        manage l x = (read x) : l

-- Problema 3: fsmap

fsmap :: a -> [a -> a] -> a
fsmap v fs = foldl (flip ($)) v fs

-- Problema 4: Dividir i vèncer

-- a és el tipus del problema, b és el tipus de la solució, i divideNconquer base divide conquer x utilitza:

-- base :: (a -> Maybe b) per obtenir la solució directa per a un problema si és trivial (quan és un Just b) o per indicar que no és trivial (quan és Nothing).
-- divide :: (a -> (a, a)) per dividir un problema no trivial en un parell de subproblemes més petits.
-- conquer :: (a -> (a, a) -> (b, b) -> b) per, donat un problema no trivial, els seus subproblemes i les seves respectives subsolucions, obtenir la solució al problema original.
-- x :: a denota el problema a solucionar.

divideNconquer ::Show a => (a -> Maybe b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b
divideNconquer base divide conquer x 
    | isNothing b = conquer x (l,r) (ls,rs)
    | otherwise = extract b
        where
            b = base x
            
            isNothing :: Maybe c -> Bool
            isNothing Nothing = True
            isNothing _ = False

            extract :: Maybe c -> c
            extract (Just z) = z

            (l,r) = divide x
            ls = divideNconquer base divide conquer l
            rs = divideNconquer base divide conquer r


quickSort :: [Int] -> [Int]
quickSort x = divideNconquer baseQS divideQS conquerQS x

baseQS :: [Int] -> Maybe [Int]
baseQS [] = Just []
baseQS [x] = Just [x]
baseQS [x,y] = Just [(min x y), (max x y)]
baseQS _ = Nothing

divideQS :: [Int] -> ([Int], [Int])
divideQS (x:xs) = (smaller, bigger)
    where
        smaller = [y | y <- xs, y <= x]
        bigger = [y | y <- xs, y > x]

conquerQS :: [Int] -> ([Int], [Int]) -> ([Int], [Int]) -> [Int]
conquerQS (x:_) _ (smaller, bigger) = smaller ++ [x] ++ bigger 


-- Problema 5: Racionals

data Racional = Rac Integer Integer

instance Eq Racional where
    (Rac n1 d1) == (Rac n2 d2) = n1*d2 == n2*d1

instance Show Racional where
    show (Rac n d) = (show $ n `div` c) ++ "/" ++ (show $ d `div` c)
        where
            c = gcd n d

racional :: Integer -> Integer -> Racional
racional x y = Rac x y

numerador :: Racional -> Integer
numerador (Rac x y) = x `div` (gcd x y)

denominador :: Racional -> Integer
denominador (Rac x y) = y `div` (gcd x y)


-- Problema 6: Arbre de Calkin-Wilf

data Tree a = Node a (Tree a) (Tree a)
recXnivells :: Tree a -> [a]
recXnivells t = recXnivells' [t]
    where 
    recXnivells' ((Node x fe fd):ts) = x:recXnivells' (ts ++ [fe, fd])


racionals :: [Racional]
racionals = recXnivells calkinWilf

calkinWilf :: Tree Racional
calkinWilf = calkinWilf' (Rac 1 1)
        where 
            calkinWilf' :: Racional -> Tree Racional
            calkinWilf' (Rac x y) = Node (racional x y) (calkinWilf' (racional x (x+y))) (calkinWilf' (racional (x+y) y))
