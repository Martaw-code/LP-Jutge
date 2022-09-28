--1.
myMap :: (a -> b) -> [a] -> [b]
myMap f xs = [f x | x <- xs]

--2.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = [x | x <- xs, p x]

--3.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [f x y | (x,y) <- zip xs ys]

--4.
thingify :: [Int] -> [Int] -> [(Int, Int)]
thingify xs ys = [(x,y) | x <- xs, y <-ys, mod x y == 0]

--5.
factors :: Int -> [Int]
factors x = [y | y <- [1..x], mod x y == 0]
