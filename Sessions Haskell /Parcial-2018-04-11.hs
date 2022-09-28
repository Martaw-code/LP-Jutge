--Apartat 1: Llista infinita
-- https://github.com/Diviloper/LP/blob/master/Haskell/Past_Exams/P91910.hs

-- https://github.com/felixarpa/LP-Haskell/tree/master/more

-- https://github.com/llop/GRAU-LP/tree/master/Haskell/MesProblemes

-- https://jutge.org/doc/haskell-cheat-sheet.pdf

-- https://jpetit.jutge.org/haskell/funcions-ordre-superior-aplicacions.html#9

multEq :: Int -> Int -> [Int]
multEq x y = [x^p * y^p | p <-[0..]]

--Apartat 2: Selecció
selectFirst :: [Int] -> [Int] -> [Int] -> [Int] 
selectFirst l1 l2 l3 = [x | x <- l1, (contains l2 x) && ( (not $ contains l3 x) || ((position l2 x) < (position l3 x)) )]
    where
        contains :: [Int] -> Int -> Bool
        contains [] _ = False
        contains (x:xs) y 
            | x == y = True
            | otherwise = contains xs y
        position :: [Int] -> Int -> Int
        position l x = pos l x 0
            where
                pos :: [Int] -> Int -> Int -> Int
                pos (y:l) x c
                    | x == y = c
                    | otherwise = pos l x (c+1)

-- Apartat 3: iterate amb scanl
myIterate :: (a -> a) -> a -> [a]
myIterate f base = scanl (\x _ -> f x) base [0..]

--Apartat 4: Taula de símbols
type SymTab a = String -> Maybe a

empty :: SymTab a
empty = \_ -> Nothing

get :: SymTab a -> String -> Maybe a
get = ($)

set :: SymTab a -> String -> a -> SymTab a
set taula clau valor c
    | clau == c = Just valor
    | otherwise = taula c
    
-- Apartat 5: Expressions amb símbols
data Expr a
     = Val a
     | Var String
     | Sum (Expr a) (Expr a)
     | Sub (Expr a) (Expr a)
     | Mul (Expr a) (Expr a)
     deriving Show
     
eval :: (Num a) => SymTab a -> Expr a -> Maybe a
eval _ (Val val) = Just val
eval taula (Var var)  = get taula var
eval taula (Sum e1 e2) = do
    ev1 <- eval taula e1
    ev2 <- eval taula e2
    return (ev1 + ev2)
eval taula (Sub e1 e2) = do
    ev1 <- eval taula e1
    ev2 <- eval taula e2
    return (ev1 - ev2)
eval taula (Mul e1 e2) = do
    ev1 <- eval taula e1
    ev2 <- eval taula e2
    return (ev1 * ev2)
    
    
--
