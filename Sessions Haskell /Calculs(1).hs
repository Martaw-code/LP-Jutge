sumMultiples35 :: Integer -> Integer
sumMultiples35 n = (sumMultiples 3 n) + (sumMultiples 5 n) - (sumMultiples 15 n)

sumMultiples :: Integer -> Integer -> Integer
sumMultiples x n = x * n' * (n' + 1) `div` 2
    where n' = div (n - 1) x

--s'hauria d'implementar el quickfib
fibonacci :: Int -> Integer
fibonacci n = fst (auxFib n)
    where
      auxFib :: Int -> (Integer, Integer)
      auxFib 0 = (0, 1)
      auxFib n 
        | even n = (f, f1)
        | otherwise = (f1, f + f1)
        where
          (a, b) = auxFib (div n 2)
          f = a * (b * 2 - a)
          f1 = a * a + b * b

sumEvenFibonaccis :: Integer -> Integer
sumEvenFibonaccis n = sum $ filter even $ takeWhile (< n) $ fibs' 0 1
  where
    fibs' :: Integer -> Integer -> [Integer]
    fibs' m n = m : fibs' n (m+n)
    
largestPrimeFactor :: Integer -> Integer
largestPrimeFactor n = head $ filter (isPrime) $ filter (isFactor n) $ alReves n

alReves :: Integer -> [Integer]
alReves n = takeWhile (>0) $ iterate (sub) (n)

sub :: Integer -> Integer
sub x = x - 1

isFactor :: Integer -> Integer -> Bool
isFactor x y = x `mod` y == 0 

isPrimeRec :: Integer -> Integer -> Bool
isPrimeRec x div
    | div == 1 = True
    | x `mod` div == 0 = False
    | otherwise = isPrimeRec x (div - 1)

isPrime :: Integer -> Bool
isPrime x
    | x == 0 = False
    | x == 1 = True
    | otherwise = isPrimeRec x (floor (sqrt (fromIntegral x)))

isPalindromic :: Integer -> Bool
isPalindromic x = s == reverse s
    where
        s = show x --ens hoo passa a string
