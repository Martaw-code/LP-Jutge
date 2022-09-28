--1.
insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert(x:xs) y
        | y <= x = y:(x:xs)
        | otherwise = x:(insert xs y)


isort :: [Int] -> [Int] 
isort [] = []
isort(x:xs) = insert (isort xs) x

--2.
remove :: [Int] -> Int -> [Int]
remove [] _  = []
remove(x:xs) y
        |x == y = xs
        | otherwise = x:remove xs y
        

ssort :: [Int] -> [Int]
ssort [] = []
ssort xs = minim:(ssort queda)
    where
        minim = minimum xs
        queda = remove xs minim
        
--3.
merge :: [Int] -> [Int] -> [Int]
merge [][] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
            | x <= y   = x:merge xs (y:ys)
            |otherwise = y:merge (x:xs) ys
            
--4.
msort :: [Int] -> [Int]
msort [] = []
msort [x] = [x]
msort xs = merge (msort primeraMeitat) (msort segonaMeitat)
    where
        primeraMeitat = take (div (length xs) 2) xs
        segonaMeitat = drop (div (length xs) 2) xs
        
qsort :: [Int] -> [Int]
qsort [] = []
qsort (p:xs) = genQsort (p:xs)
        
--5.
genQsort :: Ord a => [a] -> [a]
genQsort [] = []
genQsort(p:xs) = (genQsort menors) ++ [p] ++ (genQsort majors)
    where
        menors = [x | x <- xs, x < p]
        majors = [x | x <- xs, x >= p]
