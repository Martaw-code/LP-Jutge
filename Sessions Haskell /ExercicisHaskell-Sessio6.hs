-- main :: IO()
-- 
-- main = do
--     putStrLn "Com et dius?"
--     nom <- getLine
--     putStrLn $ "Hola " ++ nom ++ "!"
--     
--     
main :: IO()

-- main = do
--     x <- getLine --getLine es una acció que ens llegeix la seguent linia de l'entrada i l'estat del programa a l'execuatr l'acció canvia i ens torna un IO String i per obtenir l'sttring l'anomenen x i l'exraiem a través de la fletxeta <- recollim el resultat d'una accio que executa entrada sortida i amb = recollim el resultat d'una funció
--     let y = reverse x
--     putStrLn x
--     putStrLn y
    
-- main = do
--     line <-getLine
--     if line /= "*" then do 
--         putStrLn $ reverse line
--         main
--     else
--         return () --fica un valor d'un d'una mònade    
    
--llegim tot el contingut de l'entrada

-- main = do
--     contents <- getContents
--     --mapM_ (putStrLn . reverse)(lines contents) --mapM_ retorna un IO buit
--     mapM (putStrLn . reverse)(lines contents)
--     return()
    
    
-- Solucio alterantiva
main = interact reverse --interact és una acció predefinida d'ordre superior que va llegint línies i per a cadascuna escriu el resultat de cridar el paràmetre sobre ella.
