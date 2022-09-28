--1.
countIf :: (Int -> Bool) -> [Int] -> Int
countIf p xs = sum $ map (const 1) (filter p xs)

--2.
pam :: [Int] -> [Int -> Int] -> [[Int]]
pam xs fs = [map f xs | f <- fs]

--3.
pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 xs fs = map (\x -> [f x | f <- fs]) xs

--4.
filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl p f x0 xs = foldl (f) x0 (filter p xs)

--5.
insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert p [] y = [y]
insert p (x:xs) y
        | p x y = x:(insert p xs y)
        | otherwise = [y,x] ++ xs


insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort p [] = []
insertionSort p xs = foldl (insert p) [] xs
