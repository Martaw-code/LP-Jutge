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
quicksort(p:xs) = (quicksort menors) ++ [p] ++ (quicksort majors)
    where
        menors = [x | <- xs, x  <  p]
        majors = [x | <- xs, x  >=  p]
        