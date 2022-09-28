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


instance Functor Queue where
    fmap f (Queue x y) = (Queue (fmap f x) (fmap f y))

translation :: Num b => b -> Queue b -> Queue b
translation t (Queue x y) = Queue (fmap (+t) x) (fmap (+t) y)

instance Applicative Queue where
    (Queue [] [f]) <*> q = (Queue [f] []) <*> q
    (Queue [f] []) <*> q = fmap f q
    pure x = Queue [x] []


instance Monad Queue where
    return x = Queue [x] []
    (Queue x y) >>= f = foldl merge create (map f (x ++ (reverse y)))
    
merge :: Queue a -> Queue a -> Queue a
merge (Queue xf xs) (Queue yf ys) = Queue (xf ++ (reverse xs)) (yf ++ (reverse ys))

kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter p q = do
    v <- q
    if p v then return v else create
    
    
