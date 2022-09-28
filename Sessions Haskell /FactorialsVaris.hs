--recursivitat
fact1 :: Integer -> Integer
fact1 0 = 1
fact1 1 = 1
fact1 n = n *fact1(n-1)

--llistes infinites
fact2 :: Integer -> Integer
fact2 n = product [x | x <-[1..n]]

--if-then-else
fact3 :: Integer -> Integer
fact3 n = if (n <= 1) then 1 else product [x | x <-[1..n]]

--guardes
fact4 :: Integer -> Integer
fact4 n
    | n == 0 = 1
    | n == 1 = 1
    | otherwise = n *fact4(n-1)
    
--foldl
fact5 :: Integer -> Integer
fact5 x = foldl (*) x [1..(x-1)]

--scanl
fact6 :: Integer -> Integer
facs = scanl (*) 1 [1..]
fact6 n = facs !! fromInteger n

fact7 :: Integer -> Integer
facAcc a 0 = a
facAcc a n = facAcc (n*a) (n-1)
fact7 = facAcc 1

fact8 :: Integer -> Integer
fact8 n = result (for init next done)
        where init = (0,1)
              next   (i,m) = (i+1, m * (i+1))
              done   (i,_) = i==n
              result (_,m) = m
              
for i n d = until d n i
