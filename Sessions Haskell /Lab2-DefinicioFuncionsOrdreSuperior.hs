--1. myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ a [] = a
myFoldl f a (b:bs) = myFoldl f (f a b) bs

--2. myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ b [] = b
myFoldr f b (a:as) = f a (myFoldr f b as)

--3. myIterate :: (a -> a) -> a -> [a]
myIterate :: (a -> a) -> a -> [a]
myIterate f a = a:myIterate f (f a)

--4. myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil :: (a -> Bool) -> (a -> a) -> a -> a
myUntil p f a = head (filter p (myIterate f a))

--5. myMap :: (a -> b) -> [a] -> [b]
myMap :: (a -> b) -> [a] -> [b]
myMap f as = [f b | b <- as] 

--6. myFilter :: (a -> Bool) -> [a] -> [a]
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p as = [a | a <- as, p a]

--7. myAll :: (a -> Bool) -> [a] -> Bool
myAll :: (a -> Bool) -> [a] -> Bool
myAll p as = and $ map p as

--8. myAny :: (a -> Bool) -> [a] -> Bool
myAny :: (a -> Bool) -> [a] -> Bool
myAny p as = or $ map p as

--9 myZip :: [a] -> [b] -> [(a, b)]
myZip :: [a] -> [b] -> [(a, b)]
myZip [] x = []
myZip x [] = []
myZip (x:xs) (y:ys) = (x,y):myZip xs ys

--10. myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f xs ys = [f x y | (x,y) <- zip xs ys]
