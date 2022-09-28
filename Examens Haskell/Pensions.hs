data Avi = Avi {
    nom :: [Char],
    edat :: Int,
    despeses :: [Int]
    } deriving (Show)
    
promigDespeses :: Avi -> Int 
promigDespeses (Avi _ _ d) = round (fromIntegral (sum d) / fromIntegral (length d))

edatsExtremes :: [Avi] -> (Int, Int)
edatsExtremes avis = (minimum ed, maximum ed)
    where
        ed = map(\(Avi _ e _) -> e) avis
        
sumaPromig :: [Avi] -> Int 
sumaPromig avis = sum $ map promigDespeses avis

maximPromig :: [Avi] -> Int
maximPromig avis = maximum $ map promigDespeses avis
    
despesaPromigSuperior :: [Avi] -> Int -> ([Char], Int)
despesaPromigSuperior avis x = (nom,e)
    where
        (Avi nom e _) = head $ filter (\a -> promigDespeses a > x) (avis ++ [(Avi "" 0 [x+1])])
    
avis = [ Avi { nom = "Joan", edat = 68, despeses = [640, 589, 573]}, Avi { nom = "Pepa", edat = 69, despeses = [710,550,570,698,645,512]}, Avi { nom = "Anna", edat = 72, despeses = [530,534]}, Avi { nom = "Pep", edat = 75, despeses = [770,645,630,650,590,481,602]} ]
