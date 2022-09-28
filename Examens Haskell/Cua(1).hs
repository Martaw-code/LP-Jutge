data Queue a = Queue [a] [a]
     deriving (Show)
     
create :: Queue a
create = Queue [] []

push :: a -> Queue a -> Queue a
push x (Queue xs ys)  = Queue xs (x:ys)

pop :: Queue a -> Queue a
pop (Queue [] []) = Queue [] []
pop (Queue [] xs) = Queue (tail $ reverse xs) []
pop (Queue xs ys) = Queue (tail xs) ys


c = push 3 (push 2 (push 1 create))

top :: Queue a -> a
top (Queue [] xs) = head $ reverse xs
top (Queue xs _) = head xs

empty :: Queue a -> Bool
empty (Queue [] []) = True
empty _  = False

instance Eq a => Eq (Queue a)
    where
        (Queue x y) == (Queue w z) = (x ++ (reverse y)) == (w ++ (reverse z))
        
secDiv :: Float -> Float -> Either String Float
secDiv x 0 = Left "divisio per 0"
secDiv x y = Right (x/y)
