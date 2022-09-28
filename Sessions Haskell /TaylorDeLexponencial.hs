factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n-1)


exps :: Float -> [Float]
exps x = zipWith (/) (iterate (*x) 1.0) (scanl (*) 1 [1..])

exponencial :: Float -> Float -> Float
exponencial x eps = sum $ takeWhile (>= eps) (exps x)
