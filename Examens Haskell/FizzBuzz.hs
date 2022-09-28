fizzBuzz :: [Either Int String]
fizzBuzz = [fizzBuzz' x | x<-[0..]]
    where
        fizzBuzz' :: Int -> Either Int String
        fizzBuzz' x
            | mod x 3 == 0 && mod x 5 == 0 = Right "FizzBuzz"
            | mod x 3 == 0 = Right "Fizz"
            | mod x 5 == 0 = Right "Buzz"
            | otherwise = Left x
