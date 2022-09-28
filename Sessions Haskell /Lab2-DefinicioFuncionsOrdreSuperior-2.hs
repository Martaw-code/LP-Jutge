--1. Feu una funció countIf :: (Int -> Bool) -> [Int] -> Int que, donat un predicat sobre els enters i una llista d’enters, retorna el nombre d’elements de la llista que satisfan el predicat.
countIf :: (Int -> Bool) -> [Int] -> Int
countIf p xs = length $ [x | x <- xs, p x]

--2. Feu una funció pam :: [Int] -> [Int -> Int] -> [[Int]] que, donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes resultant d’aplicar cada una de les funcions de la segona llista als elements de la primera llista.
pam :: [Int] -> [Int -> Int] -> [[Int]]
pam xs fs = [map f xs | f <- fs]

--3. Feu una funció pam2 :: [Int] -> [Int -> Int] -> [[Int]] que, donada una llista d’enters i una llista de funcions d’enters a enters, retorna la llista de llistes on cada llista és el resultat d’aplicar successivament les funcions de la segona llista a cada element de la primera llista.
--Nota: Qualsevol semblança amb La parte contratante de la primera parte será considerada como la parte contratante de la primera parte és pura casualitat.

pam2 :: [Int] -> [Int -> Int] -> [[Int]]
pam2 xs fs = map (\x -> [f x | f <- fs]) xs

--4. Feu una funció filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int que fa el plegat dels elements que satisfan la propietat donada.
filterFoldl :: (Int -> Bool) -> (Int -> Int -> Int) -> Int -> [Int] -> Int
filterFoldl p op x xs = foldl op x $ filter p xs

--5. Feu una funció insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int] que donada una relació entre enters, una llista i un element, ens retorna la llista amb l’element inserit segons la relació.
--Utilitzant la funció insert, feu una funció insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int] que ordeni la llista per inserció segons la relació donada.
insert :: (Int -> Int -> Bool) -> [Int] -> Int -> [Int]
insert p [] x = [x]
insert p (x:xs) y
        | p x y = x:insert p xs y
        | otherwise = [y,x] ++ xs


insertionSort :: (Int -> Int -> Bool) -> [Int] -> [Int]
insertionSort p xs = foldl (insert p) [] xs
