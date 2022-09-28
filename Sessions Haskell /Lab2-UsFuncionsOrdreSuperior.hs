--1. Feu una funció eql :: [Int] -> [Int] -> Bool que indiqui si dues llistes d’enters són iguals.
eql :: [Int] -> [Int] -> Bool
eql [] [] = True
eql xs ys = and (zipWith (==) xs ys) && length xs == length ys
    
--2. Feu una funció prod :: [Int] -> Int que calculi el producte dels elements d’una llista d’enters.
prod :: [Int] -> Int
prod = foldr (*) 1

--3. Feu una funció prodOfEvens :: [Int] -> Int que multiplica tots el nombres parells d’una llista d’enters.
prodOfEvens :: [Int] -> Int
prodOfEvens = prod . (filter even)

--4. Feu una funció powersOf2 :: [Int] que generi la llista de totes les potències de 2.
powersOf2 :: [Int]
powersOf2 = iterate (*2) 1

--5. Feu una funció scalarProduct :: [Float] -> [Float] -> Float que calculi el producte escalar de dues llistes de reals de la mateixa mida.
scalarProduct :: [Float] -> [Float] -> Float
scalarProduct xs ys = foldl (+) 0 $ zipWith (*) xs ys
