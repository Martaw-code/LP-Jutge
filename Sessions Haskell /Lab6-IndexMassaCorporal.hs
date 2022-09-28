main :: IO()

main = do
    line <- getLine
    if line /= "*" then do
        putStrLn $ imcS line
        main
    else
        return()

imcS :: String -> String
imcS s = name ++ ": " ++ imcs
    where
        (name,weight,height) = extract s
        imcs = imcToStr $ imc weight height

extract :: String -> (String,Float,Float)
extract str = (name,weight,height)
    where
        [name,weightS,heightS] = words str
        weight = read weightS
        height = read heightS

imc :: Float -> Float -> Float
imc w h = w / (h * h)

imcToStr :: Float -> String
imcToStr i
    | i < 18 = "magror"
    | i < 25 = "corpulencia normal"
    | i < 30 = "sobrepes"
    | i < 40 = "obesitat"
    | otherwise = "obesitat morbida"




    
