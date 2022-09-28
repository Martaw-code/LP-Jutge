--Funcions d'ordre superior

--La funció predefinida map aplica una funció a cada element d'una llista
--myMap es una funció d'ordre superior
myMap :: (a -> b) -> [a] -> [b] --funcio (a->b) (transforma a's en b's [a] -> [b])
myMap f [] = []
myMap f (x:xs) = f x : map f xs

-- aplica a cada element de la llista la funció (even)
-- myMap even [1,2,3,4,5] -> [False, True, False, True, False]

-- la funció predefinida (.) retorna la composicio de dues funcions:

--(.) :: (b -> c) -> (a -> b) -> (a -> c)
--(f . g) x = f(g x)


apli2 :: (a->a) -> a -> a --apli2 aplica dos cops una funció a un element
apli2 f x = f (f x)

--de forma equivalent (associativitat de la fletx és cap a la dreta)
apli22 :: (a->a) -> (a -> a)
apli22 f = f . f --composicio de f amb ella mateixa

per2 x = 2*x

--Funcions anònimes (funcions lambda) són expressions qeu representen una funció sense nom

--Diccionaris amb FOSs

--volem definir un diccionari de strings a ints amb valors per defecte usants funcions d'ordre superior

type Dict = (String -> Int) --type ens permet introduir un nom de tipus per una definició complicada, són el typedef de c++

create :: Int -> Dict
search :: Dict -> String -> Int
insert :: Dict -> String -> Int -> Dict

create = const
search = ($)
insert dict key value x
    | key == x  = value
    | otherwise = dict x 
    
--Dividir i vèncer
-- Funció d'ordre superior genèrica dIv per l'esquema de dividir i vèncer

dIv :: (a -> Bool) -> (a -> b) -> (a -> (a,a)) -> (a -> (a,a) -> (b,b) -> b) -> a -> b

-- dIv trivial directe dividir vencer x

-- trivial :: a -> Bool(ens indica amb el boolea si el problema és trivial o no) per saber si u problema es trivial
-- directe :: a -> b per solucionar directament un problema trivial
-- dividir :: a -> (a,a) per dividir un problema no trivial en un parell de subproblemes més petits
-- vencer :: a -> (a,a) -> (b,b) -> b donat un problema un no trivial i els seus dos subproblemes i les seves resepctives suubsolucins, construeix la solucio al problema original(b), i finalment tot això s'aplicarà sobre un problema inicial
-- x :: a (tipus problema) denota el problema a solicionar i b serà el tipus de la solució al problema

dIv :: (a -> Bool) -> (a -> b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b
dIv trivial directe dividir vènçer x
    | trivial x     = directe x
    | otherwise     = vènçer x (x1, x2) (y1, y2) --y1 primera subsolucio, y2 segona subsolucio, x problema original
                          where
                              (x1, x2) = dividir x
                              y1 = dIv trivial directe dividir vènçer x1
                              y2 = dIv trivial directe dividir vènçer x2
                              

dIv :: (a -> Bool) -> (a -> b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b
dIv trivial directe dividir vènçer = dIv' 
    where
        dIv' x
            | trivial x = directe x
            | otherwise = vènçer x (x1, x2) (y1, y2)
                              where
                                  (x1, x2) = dividir x
                                  y1 = dIv' x1
                                  y2 = dIv' x2


qs :: Ord a => [a] -> [a]
qs = dIv trivial directe dividir vèncer
    where
        trivial []   = True --llista es buida cert
        trivial [_]  = True --llista amb 1 element cert
        trivial _    = False 
        directe = id --retorna aquella x, es la funcio identitat
        dividir (x:xs) = (menors, majors) --es cridara si el problema no es trivial
            where menors = filter (<= x) xs
                  majors = filter (>  x) xs
        dividir' (x:xs) = partition (<= x) xs       -- equivalent amb funció predefinida
        vèncer (x:_) _ (ys1, ys2) = ys1 ++ [x] ++ ys2
