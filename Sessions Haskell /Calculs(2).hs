import Data.Char (digitToInt)
--1. La suma dels quadrats dels primers 10 naturals és 12+22+…+102=385. El quadrat de la suma dels primers 10 naturals és (1+2+…+10)2=552=3025. Per tant, la diferència entre la suma dels quadrats dels primers 10 naturals i el quadrat de la suma dels primers 10 naturals és 3025 − 385 = 2640.
--Feu una funció diffSqrs :: Integer -> Integer que, donat un natural n, retorni la suma dels quadrats dels primers n naturals i el quadrat de la suma dels primers n naturals.
diffSqrs :: Integer -> Integer
diffSqrs n = a*a-b
  where a = n*(n+1) `quot` 2 
        b = n*(n+1)*(2*n+1) `quot` 6

--2. Una tripleta pitagòrica són tres naturals (a,b,c) tals que a2+b2=c2. Feu una funció pythagoreanTriplets :: Int -> [(Int, Int, Int)] que, donat un natural n≥1, retorni la llista de totes les tripletes pitagòriques que sumin n. Cada tripleta ha d’estar ordenada de forma que a≤ b≤ c i la llista ha de estar ordenada respecte la a.
pythagoreanTriplets :: Int -> [(Int, Int, Int)]
pythagoreanTriplets n = [(x,y,z) | x <-[1..n], y <-[x..n], z<-[y..n], (x*x + y*y == z*z) && x+y+z==n]

--3. Feu una funció tartaglia :: [[Integer]] que retorni una llista infinita amb les files de del triangle de Tartaglia (també anomenat triangle de Pascal).
seguentFila :: [Integer] -> [Integer]
seguentFila row = zipWith (+) ([0] ++ row) (row ++ [0])


tartaglia :: [[Integer]]
tartaglia = iterate seguentFila [1]

--4. Feu una funció sumDigits :: Integer -> Integer que retorni la suma dels dígits d’un natural. Utilitzeu funcions d’ordre superior enlloc de recursivitat.
sumDigits :: Integer -> Integer
sumDigits n = toInteger.sum.map digitToInt $ show n

--5. Feu una funció digitalRoot :: Integer -> Integer que retorni l’arrel digital d’un natural. Utilitzeu funcions d’ordre superior enlloc de recursivitat.
digitalRoot :: Integer -> Integer
digitalRoot n = head.dropWhile (>9) $ iterate sumDigits n
