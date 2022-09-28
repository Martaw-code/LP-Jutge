fib :: Int -> Integer
fib n = fst (auxFib n)
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
