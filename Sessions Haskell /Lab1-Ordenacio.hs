--1. Feu una funció insert :: [Int] -> Int -> [Int] que, donada una llista ordenada i un element, insereixi ordenadament el nou element a la llista.
--Feu una funció isort :: [Int] -> [Int] que implementi l’algorisme d’ordenació per inserció utilitzant la funció anterior.
insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert(x:xs) y
    | x >= y = y:x:xs
    | otherwise = x:(insert xs y)
    
isort :: [Int] -> [Int]
isort [] = []
isort(x:xs) = insert (isort xs) x  

--2. Feu una funció remove :: [Int] -> Int -> [Int] que, donada una llista i un element x, elimini la primera ocurrència de x de la llista. Podeu assumir que l’element sempre és a la llista.
--Feu una funció ssort :: [Int] -> [Int] que implementi l’algorisme d’ordenació per selecció utilitzant la funció anterior.

remove :: [Int] -> Int -> [Int]
remove [] x = []
remove(x:xs) y 
    | x == y = xs
    |otherwise = x: remove xs y
    
ssort :: [Int] -> [Int]
ssort [] = []
ssort xs = minim:ssort queda
    where
        minim = minimum xs
        queda = remove xs minim

--3. Feu una funció merge :: [Int] -> [Int] -> [Int] que, donades dues llistes ordenades, les fusioni per obtenir una llista amb tots els seus elements ordenats.
--Feu una funció msort :: [Int] -> [Int] que implementi l’algorisme d’ordenació per fusió utilitzant la funció anterior.
merge :: [Int] -> [Int] -> [Int]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
        | x >= y = y:(merge (x:xs) ys)
        | otherwise = x:(merge xs (y:ys))
    
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge(msort primeraMeitat) (msort segonaMeitat)
        where 
            primeraMeitat = take (length xs `div` 2) xs
            segonaMeitat  = drop (length xs `div` 2) xs

--4. Feu una funció qsort :: [Int] -> [Int] que implementi l’algorisme d’ordenació ràpida.
qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = genQsort (p:xs)
        
--5. Generalitzeu la funció anterior per fer ara una funció genQsort :: Ord a => [a] -> [a] que ordeni llistes de qualsevol tipus.

genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort(p:xs) = (genQsort menors) ++ [p] ++ (genQsort majors)
    where
        menors = [x | x <- xs, x < p]
        majors = [x | x <- xs, x >= p]
