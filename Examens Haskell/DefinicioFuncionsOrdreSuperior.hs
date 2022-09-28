--1.
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ x0 [] = x0
myFoldl f x0 (x:xs) = myFoldl f (f x0 x) xs

--2.
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ x0 [] = x0
myFoldr f x0 (x:xs) = f x (myFoldr f x0 xs)

--3.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x:(myIterate f (f x))

--4.
myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil p f a = head (filter p (myIterate f a))

--5.
myMap :: (a -> b) -> [a] -> [b]
myMap f as = [f b | b <- as] 

--6.
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p xs = [x | x <-xs, p x]

--7.
myAll :: (a -> Bool) -> [a] -> Bool
myAll p [] = True
myAll p xs = and [True | x <-xs, p x]

--8.
myAny :: (a -> Bool) -> [a] -> Bool
myAny p xs = or [True | x <-xs, p x]

--9.
myZip :: [a] -> [b] -> [(a, b)]
myZip x [] = []
myZip [] x = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

--10.
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [f x y | (x,y) <- zip xs ys]
