--1
absValue :: Int -> Int
absValue n
    | n >= 0 = n
    | otherwise = -n
   
--2.   
power :: Int -> Int -> Int
power _ 0 = 1
power x p = x^p

--3.
mySqrt :: Int -> Int
mySqrt n =  round $ sqrt(fromIntegral n)

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | length([x | x <- [2..mySqrt(n)], mod n x == 0]) > 0 = False
    | otherwise = True
    
--4.
slowFib :: Int -> Int
slowFib n
    | n == 0 = 0
    | n == 1 = 1
    | otherwise = slowFib(n-1) + slowFib(n-2)

--5. retorna n-èsim element de la sèrie de fibonacci 
quickFibA :: Int -> (Int,Int)
quickFibA 0 = (0,1)
quickFibA 1 = (1,0)
quickFibA n = (fn1+fn2,fn1)
    where
        (fn1,fn2) = quickFibA (n-1)

quickFib :: Int -> Int
quickFib n = fst $ quickFibA n

--6. exponenciació ràpida
fastExp :: Int -> Int -> Int
fastExp _ 0 = 1
fastExp x n
      | even n = y*y
      | otherwise = y*y*x
    where
        y = fastExp x n_halved
        n_halved = div n 2


