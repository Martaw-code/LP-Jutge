main :: IO()

suma x y = x+y

main = do
    entrada <- getArgs
    let args = head entrada
    let n = (read args)::Int
    putStrLn . show $ sum [1..n]
