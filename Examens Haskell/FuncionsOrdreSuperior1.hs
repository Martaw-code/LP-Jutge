--1.
eql :: [Int] -> [Int] -> Bool 
eql [] [] = True
eql x [] = False
eql [] y = False
eql x y = and(zipWith (==) x y) && length(x) == length(y)

--2.
prod :: [Int] -> Int
prod xs = foldl (*) 1 xs

--3.
prodOfEvens :: [Int] -> Int
prodOfEvens xs = prod $ filter even xs

--4.
powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

--5.
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct [] [] = 0
scalarProduct []  x = 0
scalarProduct x  [] = 0
scalarProduct x y = sum $ zipWith (*) x y

