{-
Entrada sortida en Haskell es una mònada

main :: IO() AMB IO gestionem l'entrada i la sortida
-}

main :: IO()

main = do
    nom <- getLine
    if last nom == 'a' || last nom == 'A'
       then do
       putStrLn $ "Hola maca!"
       else putStrLn $ "Hola maco!"
       
    
