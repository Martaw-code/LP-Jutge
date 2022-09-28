--LLISTES GENERIQUES

data Llista a = Buida | a `DavantDe`(Llista a)
    deriving (Show)

l1 = Buida
l2 = 4 `DavantDe` Buida
l3 = 5 `DavantDe` l2
l4 = 7 `DavantDe` (6 `DavantDe` l3) 

-- 7 `DavantDe` (6 `DavantDe` (5 `DavantDe` (4 `DavantDe` Buida)))

llargada :: Llista a -> Int
llargada Buida = 0
llargada (cap `DavantDe` cua) = 1 + llargada cua


--Les llistes de haskell són exactament això però amb una mica de subre sintàctic

-- data [a] = [] | a:[a]
-- 
-- l5 = 3:2:4:[] 
-- 
-- length :: [a] -> Int
-- length [] = 0
-- length (x:xs) = 1 + length xs

data Maybe a = Just a | Nothing

find :: (a->Bool) -> [a] -> Maybe a
    --cerca en una llista amb un predicat
    
lookup :: Eq a => a -> [(a,b)] -> Maybe b
    -- cerca en una llista associativa
    
data Either a b = Left a | Right b

secDiv :: Float -> Float -> Either String Float
secDiv _ 0 = Left "divisió per zero"
secDiv x y = Right (x / y)
