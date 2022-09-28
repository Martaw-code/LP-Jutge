--1. Feu una funció flatten :: [[Int]] -> [Int] que aplana una llista de llistes d’enters en una llista d’enters.
flatten :: [[Int]] -> [Int]
flatten xs = foldl (++) [] xs

--2. Feu una funció myLength :: String -> Int que retorna la llargada d’una cadena de caràcters.
myLength :: String -> Int
myLength = foldl (+) 0 . map (const 1)

--3. Feu una funció myReverse :: [Int] -> [Int] que inverteix els elements d’una llista d’enters.
myReverse :: [Int] -> [Int]
myReverse xs = foldl (flip (:)) [] xs

--4. Feu una funció countIn :: [[Int]] -> Int -> [Int] que, donada una llista de llistes d’elements ℓ i un element x ens torna la llista que indica quants cops apareix x en cada llista de ℓ.
countIn :: [[Int]] -> Int -> [Int]
countIn l x = map (length) $ map (filter (== x)) l

--5. Feu una funció firstWord :: String -> String que, donat un string amb blancs i caràcacters alfabètics), en retorna la primera paraula.
firstWord :: String -> String
firstWord text =  takeWhile (/= ' ') $ dropWhile (== ' ') text
