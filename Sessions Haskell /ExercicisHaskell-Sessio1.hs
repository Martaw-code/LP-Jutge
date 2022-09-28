--Factorial es una funció que ens transforma enters en enters
factorial :: Integer -> Integer

factorial 0 = 1
factorial n = n * factorial(n-1)

--podem aplicar la funció factorial a tots els elemens de la llista de 0 a 5
--map factorial [0..5] ho fem a través de la funció map i té com a paràmetre una funció i una llista i el que fa és aplicar la funció factorial a cada element de la llista

--quicksort(un algorisme per ordenar llistes)
quicksort [] = [] --cas base
--(p:xs) llista que comença per p i continua amb el pivot
--ordenarem recursivament els mes petits
--ordenarem recursivament els majors
quicksort []     = []
quicksort (p:xs) = (quicksort menors) ++ [p] ++ (quicksort majors)
    where
        menors = [x | x <- xs, x <  p]
        majors = [x | x <- xs, x >= p]
        
--podem definir el tipus arbre binari de t, t serà els elements que tenim dins el node de l'arbre binari, o bé un arbre binari és un arbre buit o bé és un node i quan és un node té associat una informació de tipus t(element que tenim dins del node) té associat un fill esquerre i té un subarbre dret que es un subarbre binari de t's

--Creem estructura de dades d'Arbre Binari
data Arbin t = Buit
            | Node t (Arbin t) (Arbin t)

--calcular alçada de l'arbre binari
alcada :: Arbin t -> Integer
alcada Buit = 0 --alçada arbre buit és 0
alcada (Node t fe fd) = 1 + max (alcada fe) (alcada fd)

--recorregut en preordre de l'arbre binari
preordre :: Arbin t -> [t]
preordre Buit = []
preordre (Node x fe fd) = [x] ++ preordre fe ++ preordre fd

--Num a, doblar ens transforma coses d'un tipus a un tipus a, i a es de la classe num perquè fem servir la operació producte
doblar x = 2*x

--Funcions

doble :: Int -> Int --capçalera :: indica el seu tipus (donat un enter retorna un enter)
doble x = 2*x --parametre formal x i donem el resultat com una funcio en termes de x

perimere :: Int -> Int -> Int --calcula àrea rectangle
perimere alcada amplada = doble (alcada + amplada)

xOr :: Bool -> Bool -> Bool
xOr a b = (a || b) && not (a && b)

factorial2 :: Integer -> Integer --calcula el factorial d'un natural
factorial2 n = if n == 0 then 1 else n * factorial2(n-1)

--Per definir funcions moltes vegades farem servir patrons, i són més elegants que el if-then-else

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _       = True

valAbs :: Int -> Int
valAbs n
    | n >= 0  = n
    | otherwise = (-n)
    

--definicions locals: per definir noms locals en una expressió s'usa el let-in

fastExp :: Integer -> Integer -> Integer --exponenciació ràpida
fastExp _ 0 = 1
fastExp x n =
    let y   = fastExp x n_halved
        n_halved  = div n 2
    in
        if even n then y * y else y * y * x

-- el where permet definir noms en més d'una expressió
--fastExp2 :: Integer -> Integer 
--fastExp2 _ 0 = 1
--fastExp2 x n
--    | even n    = y * y
  --  | otherwise = y * y * x
    --where
      --  y  = fastExp2 x n_halved
        --n_halved = div n 2

--currificació

--prod 3 5 és el producte de dos valors i hauria de ser 3*5, però en realitat encara que sembli que producte es una funcio que te dos parametres, nomes te un parametre que es el que apliquem a la funcio producte : (prod 3) 5, aquesta retorna una nova funció que a la vegada espera un altre parametre


--Tuples
-- t = (1,'z',False)
-- t :: Num a => (a, Char, Bool)

--podem aniuar tuples!

--ens poden servir fer funcions que retornen més d'una cosa
descomposicioHoraria :: Int -> (Int,Int,Int)
descomposicioHoraria segons = (h,m,s)
    where
        h = div segons 3600
        m = div (mod segons 3600) 60
        s = mod segons 60
    
--tuples generals, ja que no tenim definides funcions d'accés fent ús dels patrons
primer :: (a,b,c) -> a
segon  :: (a,b,c) -> b
tercer :: (a,b,c) -> c

primer (a,_,_) = a
segon  (_,b,_) = b
tercer (_,_,c) = c

-- desomposicio de tuplez en patrons

distancia :: (Float,Float) -> (Float,Float) -> Float
distancia p1 p2 = sqrt((fst p1 - fst p2)^2 + (snd p1 - snd p2)^2)

--millor descompondre per patrons als propis parametres
distancia (x1,y2) (x2,y2) = sqrt((x2-x2)^2 + (y1-y2)^2)

--tambe descompondre per patrons usant noms locals
distancia p1 p2 = sqrt(sqr dx + sqr dy)
    where
        (x1,y1) = p1
        (x2,y2) = p2
        dx = x1 - x2
        dy = y1 - y2
        sqr x = x * x

-- tupla buida (unit)
--Existeix el tipus de tupla sense cap dada, que només té un possible valor: la dada buida
-- tipus: (), valor: ()
 
 
-- LLISTES

--llista buida []
--llista enters [1,2,3] [Int]
--llita de tuples de tipus(Int,String) [(1,"a"),(2,"b")]
--llista de llistes d'enters [[Int]] [[1], [1,2]]
-- el mateix que [1..5] = [1,2,3,4,5]
-- [1,3 .. 10] = [1,3,5,7,9]

--Els textos(strings) en Haskell són llistes de caràcters
--String és sinònim de [Char]

--FUNCIONS HABITUALS PER A LLISTES

--head :: [a] -> a := donada una llista d'elements ens retorna un element
--last :: [a] -> a := donada una llista d'elements ens retorna un element

--head := retorna el primer element de la llista
--last := retorna el darrer element de la llista
--si llista és buida: *Main> head []
-- *** Exception: Prelude.head: empty list


--tail :: [a] -> [a] := donada una llista d'elements ens retorna una altre llista d'elements
--init :: [a] -> [a] := donada una llista d'elements ens retorna una altre llista d'elements

--tail := és la llista xs sense el seu primer element(TOT - PRIMER ELEM)
--init := és la llista xs sense el seu darrer element(TOT MENYS ES DARRER ELEM)

--reverse :: [a] -> [a] := donada una llista ens la retorna del revés reverse xs

--length :: [a] -> Int ::= donada una llisya d'elements ens retorna la seva llargada, i tant ens fa el tipus que digui length xs

--null :: [a] -> Bool := donada una llista d'elements ens diu si la llista és buida null xs

--elem :: a -> [a] -> Bool := donat un element i una llista ens diu si x és a la llista xs elem x xs x pertanyen a la classe Eq, els tipus que tenen l'operació de comparació

--indexació (!!)

--(!!) :: [a] -> Int -> a    xs !! i és l'ièssim element de la llista xs (començant per 0)
-- ex: [1,2,3] !! 2 ----> 3

--concatenació de dues llistes: ++

-- (++) -> [a] -> [a] -> [a] := xs ++ ys és la llista resultant de concatenar ys darrera de xs

--maximum i minimum retornen respectivament el maixm i el minim d'una llista no buida!

maximum :: Ord a => [a] -> a
minimum :: Ord a => [a] -> a

-- sum retorna la suma dels elements de la llista xs (que són numeros)

-- prod retorna el productori de la llista xs (que son elements de la classe numeral)

sum     :: Num a => [a] -> a
product :: Num a => [a] -> a

fact n = product [1 .. n] --aixo es un altre nivell

and :: [Bool] -> Bool --and bs és la conjunció de la llista de booleans bs.
or  :: [Bool] -> Bool --és la disjunció de la llista de booleans bs.

--and sobre una llista no sobre dos booleans



take :: Int -> [a] -> [a] --aqui volen els n primers elements
drop :: Int -> [a] -> [a] --aqui volem els darrers elements que  començant per darrere queden despres de treen els n primers

--take n xs és el prefixe de llargada n de la llista xs.
--drop n xs és el sufixe de la llista xs quan se li treuen els n primers elements.

--i si no en tenim prou: take 4 [1,2,3] ---> [1,2,3]

-- *Main> drop 2 [1,2,3,4] -> [3,4]
-- *Main> take 2 [1,2,3,4] -> [1,2]

--zip :: [a] -> [b] -> [(a,b)] := donada una llista a i una llista b ens ajunta les llistes a i b, produeix una llista de tuples, de parells d'elements (a,b)

--zip xs ys és la llista que combina, en ordre, cada parell d'elements de xs i ys. Si en falten, es perden.

--repeat :: a -> [a] := es la llista infinita on tots els elements son x 

--concat :: [[a]] -> [a]

-- concat xs és la llista que concatena totes les llistes de xs.


-- Existeixen moltes funcions predefinides sobre llistes que s'utilitzen habitualment.

-- Exercici: implementeu vosaltres mateixos aquest funcions en termes dels contructors.

myLength :: [a] -> Int

myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myRepeat :: a -> [a]
myRepeat x = x : myRepeat x --lista infinita de totes les xss
