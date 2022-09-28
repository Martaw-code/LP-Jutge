--1. Generar la seqüència dels uns [1,1,1,1,1,1,1,1,…].
ones :: [Integer]
ones = repeat 1

--2. Generar la seqüència dels naturals [0,1,2,3,4,5,6,7…].
nats :: [Integer]
nats = [x | x <- [0..]]

--3. Generar la seqüència dels enters [0,1,−1,2,−2,3,−3,4…].
ints :: [Integer]
ints = iterate enters 0
    where
        enters :: Integer -> Integer
        enters x
            | x > 0 = -x
            | otherwise = -x + 1
            
--4. Generar la seqüència dels nombres triangulars: 0,1,3,6,10,15,21,28,…].
triangulars :: [Integer]
triangulars = scanl (+) 0 $ tail nats

--5. Generar la seqüència dels nombres factorials: [1,1,2,6,24,120,720,5040,…].
factorials :: [Integer]
factorials = scanl (*) 1 $ tail nats

--6. Generar la seqüència dels nombres de Fibonacci: [0,1,1,2,3,5,8,13,…].
fibs :: [Integer]
fibs = f 0 1
    where
        f :: Integer -> Integer -> [Integer]
        f x y = x : (f y $ x + y)
  
--7. Generar la seqüència dels nombres primers: [2,3,5,7,11,13,17,19,…].
primes :: [Integer]
primes = garbell[2..]
    where 
        garbell(p:xs) = p:garbell[x | x <- xs, mod x p /= 0]

--8. Generar la seqüència ordenada dels nombres de Hamming: [1,2,3,4,5,6,8,9,…]. Els nombres de Hamming són aquells que només tenen 2, 3 i 5 com a divisors primers.
merge :: [Integer] -> [Integer] -> [Integer]
merge (x:xs) (y:ys)
    | x < y = x : merge xs (y:ys)
    | x == y = x : merge xs ys
    | otherwise = y : merge (x:xs) ys

hammings :: [Integer]
hammings = 1 : (merge (map (*2) hammings) $ merge (map (*3) hammings) (map (*5) hammings) )


--9. Generar la seqüència mira i digues: [1,11,21,1211,111221,312211,13112221,1113213211,…].
lookNsay :: [Integer]
lookNsay = iterate count 1

count :: Integer -> Integer
count a = read $ next $ show a

next :: [Char] -> [Char]
next [] = []
next cs = (show n) ++ [pr] ++ next cua
    where 
        pr = head cs
        n = length $ takeWhile ( == pr) cs
        cua = dropWhile ( == pr) cs
        
--10. Generar la seqüència de les files del triangle de Tartaglia (també anomenat triangle de Pascal): [[1],[1,1],[1,2,1],[1,3,3,1],…]
tartaglia :: [[Integer]]
tartaglia = iterate seguentTartaglia [1]
    where
        seguentTartaglia :: [Integer] -> [Integer]
        seguentTartaglia x = zipWith (+) (0:x) (x++[0])



