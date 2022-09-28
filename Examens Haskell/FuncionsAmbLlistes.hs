--1.
myLength :: [Int] -> Int 
myLength [] = 0
myLength(_:xs) = 1 + myLength xs

--2.
myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs)
        | x >= maxim = x
        | otherwise = maxim
        where
            maxim = myMaximum xs
        
--3.
mySum :: [Int] -> Int
mySum [] = 0
mySum(x:xs) = x + mySum xs

average :: [Int] -> Float
average [] = 0.0
average(x:xs) = fromIntegral(mySum(x:xs)) / fromIntegral(myLength(x:xs))

--4.
myReverse:: [Int] -> [Int]
myReverse [] = []
myReverse(x:xs) = (myReverse xs) ++ [x]


buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome(x:xs) = myReverse(x:xs) ++ (x:xs)

--5.
remove :: [Int] -> [Int] -> [Int]
remove [] [] = []
remove [] y = []
remove x [] = x
remove (x:xs) y
        | elem x y = remove xs y
        | otherwise = x:(remove xs y)
        
--6.
myConcat :: [[Int]] -> [Int]
myConcat [] = []
myConcat(x:xs) = x ++ (myConcat xs)

flatten :: [[Int]] -> [Int]
flatten xs = myConcat xs

--7.
oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens xs = (senars,parells)
    where
        parells = [x | x <- xs, mod x 2 == 0]
        senars  = [x | x <- xs, mod x 2 /= 0]
        
--8.
mySqrt :: Int -> Int
mySqrt n = round $ sqrt(fromIntegral n)


esPrimer :: Int -> Bool
esPrimer n
    | n < 2 = False
    | length([x | x <- [2..mySqrt(n)], mod n x == 0]) > 0 = False
    | otherwise = True

primeDivisors :: Int -> [Int]
primeDivisors n = [x | x <- [2..n], mod n x == 0 && esPrimer x]
