serieCollatz :: Integer -> [Integer]
serieCollatz 1 = [1]
serieCollatz n 
    | mod n 2 == 0 = n:serieCollatz (div n 2)
    | otherwise = n:serieCollatz(3*n + 1)
    
    
collatzMesLlarga :: Integer -> Integer
collatzMesLlarga n = maximum $ map (toInteger . length . serieCollatz) [1..n]
