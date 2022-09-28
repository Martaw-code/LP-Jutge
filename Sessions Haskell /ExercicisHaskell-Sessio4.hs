--definició del tipus algebraic forma, amb diversos constructors i cadascun amb zeros o mes floats com a dades
data Forma
    = Rectangle Float Float         -- alçada, amplada
    | Quadrat Float                 -- mida
    | Cercle Float                  -- radi
    | Punt
    deriving (Show)

--podem desconstruir els tipus algebraics amb patrons
area :: Forma -> Float
area (Rectangle amplada alçada) = amplada*alçada
area (Quadrat mida) = mida*mida
area (Cercle radi) = pi*radi*radi
area (Punt) = 0

-- *Main Debug.Trace GHC.Stack> r = Rectangle 3 4
-- *Main Debug.Trace GHC.Stack> :type r
-- r :: Forma
-- *Main Debug.Trace GHC.Stack> area r
-- 12.0

--Per escriure valors algebraics, cal afegir deriving (Show) al final del tipus, i podrem veure el tipus i els valors de la dada d'aquell tipus

-- *Main Debug.Trace GHC.Stack> r = Rectangle 4 3
-- *Main Debug.Trace GHC.Stack> r
-- Rectangle 4.0 3.0

data Punt2 = Punt2 Int Int
    deriving (Show)

p1 = Punt2 2 3

--APLICACIONS DE TIPUS ALGEBRAICS
--ARBRES BINARIS

data Arbin = Buit | Node Int Arbin Arbin
    deriving (Show)
    
a1 = Node 1 Buit Buit
a2 = Node 2 Buit Buit
a3 = Node 3 a1 a2
a4 = Node 4 a3 Buit

a5 = Node 5 a4 a4

alcada :: Arbin -> Int
alcada Buit = 0
alcada (Node _ fe fd) = 1 + max (alcada fe) (alcada fd)


--ARBRES BINARIS GENÈRICS
--Els tipus algebraics també tenen polimorfisme paramètric i ara són de tipus a
data ArbinP a = BuitP | NodeP a (ArbinP a) (ArbinP a)
    deriving (Show)

a1P :: ArbinP Int
a1P = NodeP 3 (NodeP 1 BuitP BuitP) (NodeP 2 BuitP BuitP)

a2P :: ArbinP Forma
a2P = NodeP (Rectangle 3 4) (NodeP (Cercle 2) BuitP BuitP) (NodeP Punt BuitP BuitP)
    
alcadaP :: ArbinP a -> Int
alcadaP BuitP = 0
alcadaP (NodeP _ fe fd) = 1 + max (alcadaP fe) (alcadaP fd)

preordre :: ArbinP a -> [a]
preordre BuitP = []
preordre (NodeP x fe fd) = [x] ++ preordre fe ++ preordre fd

--ARBRES GENERALS GENÈRICS
data Argal a = Argal a [Argal a] --no hi ha arbre buit en els arbres generals
    deriving (Show)
    
    
a = Argal 4 [Argal 1 [], Argal 2 [], Argal 3 [Argal 5 []]]

mida :: Argal a -> Int
mida (Argal _ fills) = 1 + sum (map mida fills)

preordreG :: Argal a -> [a]
preordreG (Argal x fills) = x : concatMap preordreG fills 

-- ARBRES BINARIS DE CERCA
Abc a = Buit | Node a (Abc a) (Abc a)  --arbre binari de cerca
a = Node 1 (Node 2 Buit) (Node 3 Buit)

esborra     :: Ord a => a -> Abc a -> Abc a  -- esborrat d'un element (exercici)

buit :: Abc a  -- retorna un arbre buit
buit = Buit

cerca    :: Ord a => a -> Abc a -> Bool -- diu si un abre conté un element
cerca x Buit = False
cerca x (Node k fe fd)
    | x < k  = cerca x fe
    | x > k  = cerca x fd
    | x == k = True

insereix :: Ord a => a -> Abc a -> Abc a -- inserció d'un element
insereix x Buit = Node x Buit Buit
insereix x (Node k fe fd) = 
    | x < k  = Node k (insereix x fe) fd
    | x > k  = Node k fe (insereix x fd)
    | x == k = Node k fe fd
    
esborra :: Ord a => a -> Abc a -> Abc a    -- esborrat d'un element (exercici)
esborra _ Buit = Node _ Buit Buit
esborra x (Node k fe fd) = 
    | x < k  = Node k (esborra x fe) fd
    | x > k  = Node k fe (esborra x fd)
    | x == k = Node k Buit Buit
    
data ExprBool
    = Val Bool
    | Var Char
    | Not ExprBool
    | And ExprBool ExprBool
    | Or  ExprBool ExprBool
    deriving (Show)
type Dict = Char -> Bool

eval :: ExprBool -> Dict -> Bool
eval (Val x) d = x
eval (Var v) d = d v
eval (Not e) d = not $ eval e d
eval (And e1 e2) d = eval e1 d && eval e2 d
eval (Or  e1 e2) d = eval e1 d || eval e2 d


