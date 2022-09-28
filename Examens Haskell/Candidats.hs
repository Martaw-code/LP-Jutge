data BST a = E | N a (BST a) (BST a) deriving (Show)

insert :: Ord a => BST a -> a -> BST a
insert E n = N n E E
insert (N a l r) n
    | n < a = N a (insert l n) r
    | n > a = N a l (insert r n)
    | otherwise = N a l r

create :: Ord a => [a] -> BST a
create x = foldl insert E x

remove :: Ord a => BST a -> a -> BST a
remove E _ = E
remove (N a l r) x
    | x == a = removeRoot (N a l r)
    | x < a = N a (remove l x) r
    |otherwise = N a l (remove r x)

removeRoot :: Ord a => BST a -> BST a
removeRoot (N a E E) = E
removeRoot (N a x E) = x
removeRoot (N a E x) = x
removeRoot (N a l r) = N newa l newr
    where 
        newa = getmin r
        newr = remove r newa

contains :: Ord a => BST a -> a -> Bool
contains E a = False
contains (N x l r) a 
    | a == x = True
    | a < x = contains l a
    | otherwise = contains r a

getmax :: BST a -> a
getmax (N x _ E) = x
getmax (N _ _ r) = getmax r

getmin :: BST a -> a
getmin (N x E _) = x
getmin (N _ l _) = getmin l

size :: BST a -> Int
size E = 0
size (N a l r) = 1 + (size l) + (size r)

elements :: BST a -> [a]
elements E = []
elements (N a l r) = (elements l) ++ [a] ++ (elements r)

-- t = create [3,4,1,2]
