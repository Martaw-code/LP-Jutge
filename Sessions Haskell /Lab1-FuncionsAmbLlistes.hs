-- 1. Feu una funció myLength :: [Int] -> Int que, donada una llista d’enters, calculi la seva llargada.
myLength :: [Int] -> Int
myLength [] = 0
myLength(_:xs) = 1 + myLength xs

-- 2. Feu una funció myMaximum :: [Int] -> Int que, donada una llista d’enters no buida, calculi el seu màxim.
myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum(x:xs)
        | (x > myMaximum xs) = x
        | otherwise = myMaximum xs

-- 3. Feu una funció average :: [Int] -> Float que, donada una llista d’enters no buida, calculi la seva mitjana.
mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + mySum xs

average :: [Int] -> Float
average [] = 0
average(x:xs) = fromIntegral(mySum (x:xs))/fromIntegral(myLength(x:xs))

--4. Feu una funció buildPalindrome :: [Int] -> [Int] que, donada una llista, retorni el palíndrom que comença amb la llista invertida.
myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse(x:xs) = myReverse xs ++ [x]

buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome(x:xs) = myReverse(x:xs) ++ (x:xs)

--5. Feu una funció remove :: [Int] -> [Int] -> [Int] que donada una llista d’enters x i una llista d’enters y, retorna la llista x havent eliminat totes les ocurrències dels elements en y.
remove :: [Int] -> [Int] -> [Int]
remove [] y = []
remove (x:xs) y
    | x `elem` y = remove xs y
    | otherwise = x : (remove xs y) 

--6. Feu una funció flatten :: [[Int]] -> [Int] que aplana una llista de llistes produint una llista d’elements.
flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

--7. Feu una funció oddsNevens :: [Int] -> ([Int],[Int]) que, donada una llista d’enters, retorni dues llistes, una que conté els parells i una que conté els senars, en el mateix ordre relatiu que a l’original.
oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:xs) = (senars,parells)
    where
        parells = [x | x <- (x:xs), mod x 2 == 0] --fem (x.xs) perque sino no pilla es primer valor
        senars  = [x | x <- (x:xs), mod x 2 == 1] 

--8. Feu una funció primeDivisors :: Int -> [Int] que retorni la llista de divisors primers d’un enter estrictament positiu.
mySqrt :: Int -> Int
mySqrt n = floor (sqrt (fromIntegral n))

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | length([x | x <- [2 .. mySqrt(n)], mod n x == 0]) > 0 = False
    | otherwise = True
    
    
primeDivisors :: Int -> [Int]
primeDivisors 1 = []
primeDivisors n = llista
    where
        llista = [x | x <- [2 .. n], mod n x == 0 && isPrime x]
            
