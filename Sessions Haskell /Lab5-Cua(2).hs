data Queue a = Queue [a] [a]
    deriving (Show)

-- pop :: Queue a -> Queue a
-- top :: Queue a -> a
-- empty :: Queue a -> Bool

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


c1 = push 4 (pop (push 3 (push 2 (push 1 create))))
c2 = push 4 (push 3 (push 2 create))

--1. Feu que Queue sigui instància de la classe Functor. Per això implementeu la funció fmap que, donada una funció de tipus p -> q i un Queue d’elements de tipus p, retorna un Queue de tipus q resultant d’aplicar la funció a tots els elements de la cua.
instance Functor Queue
    where
        fmap f (Queue x y) = Queue (fmap f x) (fmap f y)
        
        
--2. Feu una funció translation :: Num b => b -> Queue b -> Queue b que aplica una translació a tots els punts d’una cua (que serà el segon paràmetre).
translation :: Num b => b -> Queue b -> Queue b
translation f (Queue x y) = fmap (+f) (Queue x y) 

--3. Feu que Queue sigui instància de la classe Monad. Per a resoldre aquest apartat, pot ser útil fer una operació que faci la unió de dues cues del mateix tipus.
instance Applicative Queue
    where
        -- (<*>) :: f (a -> b) -> (f a -> f b)
        (Queue [] [f]) <*> q = (Queue [f] []) <*> q
        (Queue [f] []) <*> q = fmap f q
        pure x = Queue [x] []

instance Monad Queue
    where
        return x = Queue [x] []
        (Queue x y) >>= f = foldl merge create (map f (x ++ (reverse y)))


merge :: Queue a -> Queue a -> Queue a
merge (Queue xf xs) (Queue yf ys) = Queue (xf ++ (reverse xs)) (yf ++ (reverse ys))
        
--4. Feu, utilitzant la notació do, una funció kfilter :: (p -> Bool) -> Queue p -> Queue p que selecciona tots els elements d’una cua que satisfan una propietat donada.
kfilter :: (p -> Bool) -> Queue p -> Queue p
kfilter f q = do
    v <- q
    if f v then return v else create
