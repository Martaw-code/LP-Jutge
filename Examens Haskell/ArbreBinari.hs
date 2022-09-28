data Tree a = Node a (Tree a) (Tree a) | Empty deriving (Show)
 
size :: Tree a -> Int
size Empty = 0
size (Node _ l r) = 1 + (size l) + size(r)

height :: Tree a -> Int
height Empty = 0
height (Node _ l r) = 1 + max (height l) (height r)

equal :: Eq a => Tree a -> Tree a -> Bool
equal Empty Empty = True
equal _ Empty = False
equal Empty _ = False
equal (Node x fe fd) (Node y fee fdd) = x == y && (equal fe fee) && (equal fd fdd)

isomorphic :: Eq a => Tree a -> Tree a -> Bool
isomorphic Empty Empty = True
isomorphic Empty _ = False
isomorphic _ Empty = False
isomorphic (Node x fe fd) (Node y fee fdd) = x==y || (isomorphic fe fee) || (isomorphic fd fdd) || (isomorphic fd fee)
 
preOrder :: Tree a -> [a]
preOrder Empty = []
preOrder (Node x l r) = [x] ++ (preOrder l) ++ (preOrder r)
 
postOrder :: Tree a -> [a] 
postOrder Empty = []
postOrder (Node x l r) = (postOrder l) ++ (postOrder r) ++ [x]

inOrder :: Tree a -> [a] 
inOrder Empty = []
inOrder (Node x l r) = (inOrder l) ++ [x] ++ (inOrder r)

--8. Feu una funció breadthFirst :: Tree a -> [a] que, donat un arbre, retorni el seu recorregut per nivells.
parents :: [Tree a] -> [a]
parents [] = []
parents l = map parent l
    where
        parent :: Tree a -> a
        parent (Node x _ _ ) = x

children :: [Tree a] ->  [Tree a]
children [] = []
children l = foldr (++) [] $ map getChildren l
    where 
        getChildren :: Tree a -> [Tree a]
        getChildren (Node a Empty Empty) = []
        getChildren (Node a x Empty) = [x]
        getChildren (Node a Empty x) = [x]
        getChildren (Node a x y) = [x,y]

breadthFirst :: Tree a -> [a]
breadthFirst Empty = []
breadthFirst a = breadthFirstAux [a]
        where 
            breadthFirstAux :: [Tree a] -> [a]
            breadthFirstAux [] = []
            breadthFirstAux l = parents l ++ (breadthFirstAux $ children l)

--9. Feu una funció build :: Eq a => [a] -> [a] -> Tree a que, donat el recorregut en pre-ordre d’un arbre i el recorregut en in-ordre del mateix arbre, retorni l’arbre original. Assumiu que l’arbre no té elements repetits.
build :: Eq a => [a] -> [a] -> Tree a
build [] [] = Empty
build [x] [y] = Node x Empty Empty
build (h:pre) ind = Node h (build lpre lin) (build rpre rin)
    where
        lin = takeWhile (/= h) ind
        lastl = last lin
        rin = tail $ dropWhile (/= h) ind
        lpre = takeWhile (/= lastl) pre ++ [lastl]
        rpre = tail $ dropWhile (/= lastl) pre
        
overlap :: (a -> a -> a) -> Tree a -> Tree a -> Tree a
overlap f Empty Empty = Empty
overlap f Empty x = x
overlap f x Empty = x
overlap f (Node x fe fd) (Node y fee fdd) = Node (f x y) (overlap f fe fee) (overlap f fd fdd)


t7 = Node 7 Empty Empty
t6 = Node 6 Empty Empty
t5 = Node 5 Empty Empty
t4 = Node 4 Empty Empty
t3 = Node 3 t6 t7
t2 = Node 2 t4 t5
t1 = Node 1 t2 t3
t1' = Node 1 t3 t2
