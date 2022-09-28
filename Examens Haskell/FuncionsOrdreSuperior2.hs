--1.
flatten :: [[Int]] -> [Int]
flatten xs = foldl (++) [] xs

--2.
myLength :: String -> Int
myLength "" = 0
myLength x = sum $ map (const 1) x 

--3.
myReverse :: [Int] -> [Int]
myReverse [] = []
myReverse xs = foldl (flip (:)) [] xs

--4.
countIn :: [[Int]] -> Int -> [Int]
countIn l x = map (length) $ map (filter (== x)) l

--5.
firstWord :: String -> String
firstWord x = head $ words x
