--Problema 1: Expressió postfixa 1



-- Problema 2: Expressió postfixa 2


-- Problema 3: fsmap
fsmap :: a -> [a -> a] -> a
fsmap x fs = foldl (flip ($)) x fs

-- Problema 4: Dividir i vèncer


-- Problema 5: Racionals

data Racional = Racional Integer Integer
    
instance Eq Racional where
    (Racional a b) == (Racional c d) = (a*d == c*b)

instance Show Racional where
    show (Racional a b) = (show $ div a d) ++ "/" ++ (show $ div b d)
        where
            d = gcd a b

racional :: Integer -> Integer -> Racional
racional a b = Racional a b

numerador :: Racional -> Integer
numerador (Racional a b) = div a (gcd a b)

denominador :: Racional -> Integer
denominador (Racional a b) = div b (gcd a b)

--Problema 6: Arbre de Calkin-Wilf

data Tree a = Node a (Tree a) (Tree a)

recXnivells :: Tree a -> [a]
recXnivells t = recXnivells' [t]
    where recXnivells' ((Node x fe fd):ts) = x:recXnivells' (ts ++ [fe, fd])

racionals :: [Racional]
racionals = recXnivells calkinWilf

calkinWilf :: Tree Racional
calkinWilf = calkinWilf' (Racional 1 1)
    where
        calkinWilf' :: Racional -> Tree Racional
        calkinWilf' (Racional a b) = Node (Racional a b) (calkinWilf' (racional a (a+b))) (calkinWilf' (racional(a+b) b))
    
