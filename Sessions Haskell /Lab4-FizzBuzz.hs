fizzBuzz :: [Either Int String]
fizzBuzz = [fizzBuzzValue x | x <- [0..]]
    where
        fizzBuzzValue :: Int -> Either Int String
        fizzBuzzValue x
            | mod x 3 == 0 && mod x 5 == 0 = Right "FizzBuzz"
            | mod x 3 == 0 = Right "Fizz"
            | mod x 5 == 0 = Right "Buzz"
            | otherwise = Left x
 
