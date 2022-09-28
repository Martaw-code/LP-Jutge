
-- 1. Nombres romans (amb recursivitat)

roman2int :: String -> Int
roman2int roman = nums2int 0 $ map translate roman
    where
        nums2int :: Int -> [Int] -> Int
        nums2int x [] = x
        nums2int x [y] = x + y
        nums2int x (y:z:l)
            | y < z = nums2int (x+(z-y)) l
            | otherwise = nums2int (x+y) (z:l)

translate :: Char -> Int
translate x = case x of
    'I' -> 1
    'V' -> 5
    'X' -> 10
    'L' -> 50
    'C' -> 100
    'D' -> 500
    'M' -> 1000
    x -> 0


-- 2. Nombres romans (sense recursivitat)
roman2int' :: String -> Int
roman2int' roman = nums2int' $ map translate roman
    where
        nums2int' :: [Int] -> Int
        nums2int' nums = sum $ zipWith sumOrSubs (nums) (tail nums ++ [0])
            where
                sumOrSubs :: Int -> Int -> Int
                sumOrSubs x next
                    | x >= next = x
                    | otherwise = -x


-- 3. Arrels

arrels :: Float -> [Float]
arrels x = scanl taylor x $ repeat x
    where 
        taylor :: Float -> Float -> Float
        taylor prev x = 0.5 * (prev + (x/prev))


-- 4. Més arrels

arrel :: Float -> Float -> Float
arrel x error = arr error $ arrels x
    where
        arr :: Float -> [Float] -> Float
        arr err (x:y:l)
            | abs (x - y) <= err = y
            | otherwise = arr err (y:l)


-- 5. Escriptura d’arbres

data LTree a = Leaf a | Node (LTree a) (LTree a)

instance Show a => Show (LTree a) where
    show (Leaf x) = "{" ++ show x ++ "}"
    show (Node l r) = "<" ++ (show l) ++ "," ++ (show r) ++ ">"


-- 6. Creació d’arbres equilibrats

build :: [a] -> LTree a
build x = creaArbre $ map Leaf x
    where
        creaArbre :: [LTree a] -> LTree a
        creaArbre [x] = x
        creaArbre x = Node (creaArbre $ take l x) (creaArbre $ drop l x)
            where
                l = div (length x + 1) 2 


-- 7. Mònades i arbres

zipLTrees :: LTree a -> LTree b -> Maybe (LTree (a,b))
zipLTrees (Leaf _) (Node _ _) = Nothing
zipLTrees (Node _ _) (Leaf _ ) = Nothing
zipLTrees (Leaf x) (Leaf y) = Just $ Leaf (x,y)
zipLTrees (Node l1 r1) (Node l2 r2) = do
    l <- zipLTrees l1 l2
    r <- zipLTrees r1 r2
    return $ Node l r
