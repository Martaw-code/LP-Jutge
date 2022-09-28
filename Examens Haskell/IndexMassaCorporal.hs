
main :: IO()


main = do
    line <- getLine
    if line /= "*" then do
        putStrLn $ imcs line
        main
    else
        return ()
        
imcs :: String -> String
imcs line = name ++ ": " ++ res
    where
        (name,w,h) = extract line
        res = imcToString $ imc w h

extract :: String -> (String, Float, Float)
extract s = (nom,nouW,nouH)
    where
        [nom,w,h] = words s
        nouW = (read w)::Float
        nouH = (read h)::Float
    
imc :: Float -> Float -> Float
imc w h = w / (h*h)

imcToString :: Float -> String
imcToString n
        | n < 18 = "magror"
        | n < 25 = "corpulencia normal" 
        | n < 30 = "sobrepes" 
        | n < 40 = "obesitat"
        | otherwise =  "obesitat morbida" 
