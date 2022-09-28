data STree a = Nil | Node Int a (STree a) (STree a)
    deriving (Show)

talla :: STree a -> Int
talla Nil = 0
talla (Node t _ _ _ ) = t
    
    
isOk :: STree a -> Bool 
isOk Nil = True
isOk (Node t _ l r) = (isOk l) && (isOk r) && (t == (talla l) + (talla r) + 1)

nthElement :: STree a -> Int -> Maybe a
nthElement (Node t x l r) n
    | n > t || n < 1 = Nothing
    | n <= talla l = nthElement l n
    | n == talla l + 1 = Just x
    | otherwise = nthElement r (n - talla l -1)
    

mapToElements :: (a -> b) -> STree a -> [Int] -> [Maybe b]
mapToElements f t n = map apply n
    where
        apply nE = do
            el <- nthElement t nE
            return (f el)


div10 = flip div 10


instance Functor STree where
    fmap f Nil = Nil
    fmap f (Node t x l r) = Node t (f x) (fmap f l) (fmap f r)
