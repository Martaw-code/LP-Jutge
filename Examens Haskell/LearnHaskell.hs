--haskell és un llenguatge de programamció funcional pura

--Per tant no tenim: ni assignacions, bucles o efectes laterals, ni tampoc fem gestió explícita de la memòria

--tenim avaluació lazy, podem passar i retornar funcions i ens infereix el tipus de dades, tot i que és millor donar nosaltres la capçalera de les funcions

-- :type objecte, podem mirar el tipus de l'objecte del qual es tracta

--definició de factorial
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n-1)

--en canvi si apliquem la funció factorial a cadascun dels elements d'una llista ens convindria fer servir la funció: map

{-
*Main> :doc map
 \(\mathcal{O}(n)\). 'map' @f xs@ is the list obtained by applying @f@ to
 each element of @xs@, i.e.,

 > map f [x1, x2, ..., xn] == [f x1, f x2, ..., f xn]
 > map f [x1, x2, ...] == [f x1, f x2, ...]

 >>> map (+1) [1, 2, 3]
 
-}

--com podem veure dins de les llistes de comprensió mirarem si els elements són majors o menors al pivot, per tant el que este, fent al cap i a la fí són comparacions entre els elements. Com que estem fent comparacions, podem generalitzar el tipus dels elements de quicksort, els quals seran de la classe Or(:= ordenable)
quicksort :: Ord t => [t] -> [t]
quicksort [] = []
quicksort(p:xs) = (quicksort menors) ++ [p] ++ (quicksort majors)
    where
        menors = [x | x <- xs, x < p]
        majors = [x | x <- xs, x >= p]
        

--arbres binaris
data Arbin t = Buit | Node t (Arbin t) (Arbin t)

-- un arbre binari de tipus t pot ser buit, o bé esser format per nodes de tipus t amb els corresponents subarbres drets i esquers

alcada :: Arbin t -> Integer
alcada Buit = 0
alcada (Node t l r) = 1 + max (alcada l) (alcada r) 

-- una cerca en preordre de l'arbre primer mirem el node, després el subarbre esquerra i després el subarbre dret
preordre :: Arbin t -> [t]
preordre Buit = []
preordre (Node t l r) = [t] ++ (preordre l) ++ (preordre r)

{------------TIPUS BÀSICS-----------}

-- Booleans:   Bool: False, True

-- Enters:     Int(64 bits en Ca2), Integer(molt gran)
    --Operacions: +, -, *, div, mod, rem, ^ 
    --Operadors: >= , <= , >, <, ==, /=

-- Reals:      Float(Reals de coma floatant de 32 bits),
            -- Double(Reals de coma flotant de 64 bits)
    -- Operacions: +, -, *, /, **
    -- Operadors: >, <, >= , <=, ==, /=
    
    --Convertir un enter a real: fromIntegral
    --Convertir un real a enter: floor, round, ceiling

-- Caraàcters: Char
-- Funcions de conversió: cal un import Data.Char
--chr :: Int -> Char
--ord :: Char -> Int

--funcions predefinides habituals
--parell/senar(False)
myEven :: Integer -> Bool
myEven x
    | mod x 2 == 0 = True
    | otherwise    = False
    
--minim/maxim
myMin :: Integer -> Integer -> Integer
myMin x y
    | x >= y    = y
    | otherwise = x
    
--maximComuDivisor(gcd), minimComuMultiple(mcm)
{-gcd :: Integral a => a -> a -> a
  lcm :: Integral a => a -> a -> a-}
  
{- funcions matemàtiques
abs  :: Num a      => a -> a
sqrt :: Floating a => a -> a
log  :: Floating a => a -> a
exp  :: Floating a => a -> a
cos  :: Floating a => a -> a
-}

--FUNCIONS: Les funcions no tenen efectes laterals i per tant no modifiquen paràmetres, no modifiquen la memòria ni l'entrada/sortida

--Les funcions són pures: només retornen resultats calculats en relació als seus paràmetres. I sempre retornará el mateix aplicada sobre els mateixos paràmetres

--Per declarar una funció: 

-- capçalera(declaració de tipus de la funcio)
-- definició(fent servir els paràmetres)

doble :: Int -> Int
doble = (2*) --quedaria doble x = 2 * x ---> doble = (2*)

perimetre :: Int -> Int -> Int
perimetre alcada amplada = 2*(amplada+alcada)

perimetre2 :: Int -> Int -> Int
perimetre2 alcada amplada = doble $ amplada+alcada

xOr :: Bool -> Bool -> Bool --o exclusiva
xOr a b = (a || b) && not(a && b)

factorial2 :: Integer -> Integer
factorial2 n = if n == 0 then 1 else n*factorial2(n-1)

--definicions amb patrons: son els diferents casos que podem tenir un a sota d'altre i retorna el resultat de la primera branca que casa. L'avaluaciódels patrons és de dalt a baix. Com hem definir abans la funció de factorial n'és un exemple

-- Els patrons els considerem molt més elegants que el if-then-else i tenen moltes més aplicacions

-- _ representa una variable anònima

nand :: Bool -> Bool -> Bool
nand True True = False
nand _ _       = True

-- o també podem definir funcions amb guardes

valAbs :: Int -> Int
valAbs n
    | n >= 0 = n
    | otherwise = -n --otherwise és el mateix que el True però més llegible
    
-- L'avaluació de guardes també és de dalt a baix i retorna el resultat de la primera branca que cada. Error si cap és certa

--Les definicions per patrons també poden tenir guardes


-- definició de noms locals: podem fer servir el let-in o el where
fastExp :: Integer -> Integer -> Integer
fastExp _ 0 = 1
fastExp x n =
    let y = fastExp x n_halved
        n_halved = div n 2
    in
        if even n then y*y else y*y*x
        
fastExp2 :: Integer -> Integer -> Integer --exponenciacio rapida
fastExp2 _ 0 = 1
fastExp2 x n
    | even n = y*y
    | otherwise = y*y*x
    where
        y = fastExp2 x n_halved
        n_halved = div n 2
        
        
fastExp3 :: Int -> Int -> Int
fastExp3 _ 0 = 1
fastExp3 x n 
    | even n = y*y
    | otherwise = y*y*x
    where
        y = fastExp3 x n_halved
        n_halved = div n 2
        

--CURRIFICACIÓ

{-
Totes les funcions tenen un únic paràmetre. I les funcions que tenen més d'un paràmetre en realitat retornen una nova funció. noo cal que passem tots els paràmetres(aplicació parcial)

a -> b -> c ---> a -> (b -> c)

f x y --> (f x) y

-}



prod :: Integer -> Integer -> Integer
prod x y = x*y

-- Primer apliquem x i el resultat és una funció que espera un altre enter
--prod :: Integer -> (*Integer* -> Integer)

-- (prod 3) :: (*Integer* -> Integer)

-- (prod 3) 5 :: Int

-- INFERÈNCIA DE TIPUS

-- si no donem la capçalera d'una funció, Haskell infereix el seu tipus

-- NOTACIÓ PREFEIXA/INFIXA

-- Els operadors infixes -> posar-los entre parèntesis per fer-los prefixes ---> 2+3 passem a (+) 2 3

-- Les funcions prefixdes -> posales entre backtits per fer-les infixes ----> div 8 4 ->>> 8 `div` 4

-- TUPLES
{-
Una tupla és un valor estructurat que permet desar diferents valors de tipus, i per tant guarda tipus heterogènis de valors de tipus t1,t2,t3,...,tn en un únic valor de tipus (t1,t2,...,tn)

El nombre de camps es fix
Els camps son de tipus heterogènis
-}

z = (3,'z',False) -- z :: (Integer, Char, Bool)

descomposicioHoraria :: Int -> (Int,Int,Int)
descomposicioHoraria x = (hores,minuts,segons)
    where
        hores  = div x 3600
        minuts = div (mod x 3600) 60
        segons = mod (div (mod x 3600) 60) 60

--Accés a tuples

-- Per a tuples de dos elements, poden accedir amb fst i snd
{-
*Main Data.Char> z = (1,"a")
*Main Data.Char> fst z
1
*Main Data.Char> snd z
"a"
-}

--Per a tuples generals, no hi ha definides funcions d'accés peròo ens les pdem crear fent servir patrons

primer :: (a,b,c) -> a
primer (a,_,_) = a

segon :: (a,b,c) -> b
segon (_,b,_) = b

tercer :: (a,b,c) -> c
tercer (_,_,c) = c

--descomposicio de tuples en patrons
--descomposem per patrons als propis parametres
distancia :: (Float,Float) -> (Float,Float) -> Float
distancia (x1,y1) (x2,y2) = sqrt((x1-x2)^2 + (y1-y2)^2)

--descomposem usant noms locals
distancia2 :: (Float,Float) -> (Float,Float) -> Float
distancia2 p1 p2 = sqrt(sqr dx + sqr dy)
    where
        (x1,y1) = p1
        (x2,y2) = p2
        dx      = x1-x2
        dy      = y1-y2
        sqr x   = x*x
        
--Tupla buida(unit)

--Existeix el tipus de tupla sense cap dada, que només té un possible valor: la dada buida

--Concepte semblant al void de C: Tipus:(), Valor:()


--Llistes

{-
Una llista és un tipus estructurat que conté una seqüència d'elements tots del mateix tipus

[t] denota la llista amb elements de tipus t



-- CONSTRUCTOR DE LLISTES

Les llistes tenen dos constructors: ":" i "[]"

- La llista buida: [] :: [a]

- Afegir per davant: (:) :: a -> [a] -> [a]

--[16,12,21] --> 16 : [12,21] -> 16 : 12 : 21 : []

-- que vol dir 16 : (12 : (21 : []))

les llistes de Haskell són llistes simplement encadenades

[] i : funcionen en temps constant

l1 = 3 : 2 : 1 : []
l2 = 4 : l1

L'operador ++ retorna la concatenació de dues llistes (temps proporcional a la llargada de la primera llista)

Per tant o bé una llista és buida o bé té un element seguit d'una subllista
-}

--TEXTOS

--Els textos en Haskell són llistes de caràcters
--Els tipus String és un sinònim de Char

nom1 :: [Char]
nom1 = "marta"

nom2 :: [Char]
nom2 = 'a':'b':'c':[]


--FUNCIONS HABITUALS PER A LLISTES --

--HEAD 

--head :: [a] -> a

--head xs és el primer element de la llista xs

--LAST

--last :: [a] -> a

-- last xs és el darrer element de la llista xs

-- Error si xs és buida

--TAIL

-- tail :: [a] -> [a]

-- tail xs és la llista xs sense el seu primer element

-- INIT

-- init :: [a] -> [a]

-- init xs és la llista xs sense el seu darrer element


-- REVERSE

-- reverse :: [a] -> [a]

--reverse xs és la llista xs del revés

--LENGTH

--length :: [a] -> Integer

--length xs és el nombre d'elements de la llista xs

-- NULL

-- null :: [a] -> Bool

-- null xs indica si la llista xs és buida

-- ELEM

-- elem x xs indica si x és a la llista xs

-- (!!)

--(!!) :: [a] -> Int -> a

-- xs !! i és l'i-èssim element de la llista xs (començant per 0)

--(++) concatenació de dues llistes

-- (++) :: [a] -> [a] -> [a]

-- xs ++ ys és la llista resultant de concatenar xs i ys

--FUNCIONS D'ORDRE SUPERIOR

--UNA FUNCIÓ D'ORDRE SUPERIOR ES UNA FUNCIO QUE REP O RETORNA FUNCIONS(FOS)

--map per exemple es una funció predefinida que aplica a cada element d'una llista una funcio

map :: (a->b) -> [a] -> [b]

map f [] = []
map f (x:xs) = f x : map f xs

--(.) la funció (.) retorna la composició de dues funcions
(.) :: (b -> c) (a -> b) -> (a -> c)
(f . g) x = f (g x)

apli2 :: (a->a) -> a -> a --aplica una funció dos cops a un element
apli2 f x = f (f x)

apli2 :: (a->a) -> (a->a)
apli2 f = f . f

--FUNCIONS ANÒNIMES
{-
Les funcions anònimes(o lambda) són expressioons que representen una funció sense nom. Permten definir directament expressions que no deixen de ser funcions

\x -> x + 3 es una funció anònima tal que donada una x retorna x+3

(\x -> x+3) 4 aplica la funció anonima sobre 4
-}

-- podem fer servir múltiples paràmetres

\x y -> x + y equivalent a \x -> \y -> x+y que es \x -> (\y -> x+y)

--LES SECCIONS PERMETEN APLICAR OPERADORS INFIXOS PARCIALMENT i són aplicacions parcials d'operadors binaris

Per la dreta: (*y) eq a \x -> x * y
Per l'esquerra: (y*) eq a \x -> y * x

-- FUNCIÓ DE DIVIDIR I VÈNCER 

--Funció d'ordre superior genèrica dIv per l'esquema de dividir i vèncer

dIv :: (a -> Bool) -> (a -> b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b

a és el tipus del problema i b és el tipuus de la solució

dIv trivial directe dividir vènçer x

trivial :: (a -> Bool) per saber si un problema és trivial

directe :: (a -> b) per solucionar directament un problema

dividir :: (a -> (a,a)) per dividir un problrma no trivial en un parell de subproblemes més petits

vencer :: (a -> (a,a) -> (b,b) -> b) donat un problema no trivial, els seus respectius subproblemes i les seves respectives subsolucions, obtenim la solució al problema

x :: a denota el problema a solucionar i es de tipus a

--solucionar quicksort per dividir i vencer

dIv :: (a -> Bool) -> (a -> b) -> (a -> (a, a)) -> (a -> (a, a) -> (b, b) -> b) -> a -> b

dIv trivial directe dividir vencer x
    | trivial x = directe x
    | otherwise = vencer x (x1,x2) (y1,y2)
        where
            (x1,x2) = dividir x
            y1 = dIv trivial directe dividir vencer x1
            y2 = dIv trivial directe dividir vencer x2













