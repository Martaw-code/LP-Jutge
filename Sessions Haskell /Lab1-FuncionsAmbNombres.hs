--1. Feu una funció absValue :: Int -> Int que, donat un enter, retorni el seu valor absolut.
absValue :: Int -> Int
absValue n = if n >= 0 then n else (-n)

--2. Feu una funció power :: Int -> Int -> Int que, donats un enter x i un natural p, retorni x elevat a p, és a dir, xp.
power :: Int -> Int -> Int
power _ 0 = 1
power x p = x^p

--3. Feu una funció isPrime :: Int -> Bool que, donat un natural, indiqui si aquest és primer o no.
mySqrt :: Int -> Int
mySqrt n = floor (sqrt (fromIntegral n))

isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | length([x | x <- [2 .. mySqrt(n)], mod n x == 0]) > 0 = False
    | otherwise = True

--4. Feu una funció slowFib :: Int -> Int que retorni l’n-èsim element de la sèrie de Fibonacci tot utilitzant l’algorisme recursiu que la defineix (f(0)=0, f(1)=1, f(n)=f(n−1)+f(n−2) per n≥ 2).
slowFib :: Int -> Int
slowFib n
    |n == 0 = 0
    |n == 1 = 1
    |otherwise = slowFib(n-2) +slowFib(n-1)

--5. Feu una funció quickFib :: Int -> Int que retorni l’n-èsim element de la sèrie de Fibonacci tot utilitzant un algorisme més eficient.
quickFibA :: Int -> (Int, Int)
quickFibA 0 = (0, 0)
quickFibA 1 = (1, 0)
quickFibA n = (fn1+fn2, fn1)
        --O(log n)
        where (fn1, fn2) = quickFibA (n-1)

quickFib :: Int -> Int
quickFib n = fst (quickFibA n) --fst on es troba l'nèssim, snd és el n-1 èssim
