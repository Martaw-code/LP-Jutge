data AVL a = E | N a Int (AVL a) (AVL a) deriving (Show)

insert :: Ord a => AVL a -> a -> AVL a
insert tree x = balance $ insertAux tree x

create :: Ord a => [a] -> AVL a
create l = foldl insert E l

check :: AVL a -> (Bool,Int)
check E = (True, -1)
check (N r h lc rc)
    | lcorrect && rcorrect && heightDiff <=1 = (True, h)
    | otherwise = (False, -99)
    where
        (lcorrect, lheight) = check lc
        (rcorrect, rheight) = check rc
        heightDiff = abs $ lheight - rheight
