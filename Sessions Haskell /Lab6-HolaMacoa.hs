main :: IO()

main = do
    name <- getLine
    putStrLn $ hello name
    
hello :: String -> String
hello name
    | esFemeni name = "Hola maca!"
    | otherwise = "Hola maco!"
    where
        esFemeni :: String -> Bool
        esFemeni name = last name == 'a' || last name == 'A'
        
    
    
