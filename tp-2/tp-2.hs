--1. Recursión sobre listas

-- 1. 
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

--2.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3. 
sucesores :: [Int] -> [Int]
sucesores []= []
sucesores (x:xs)= (x+1) : (sucesores xs) 

--4. 
conjuncion :: [Bool] -> Bool
conjuncion []= True
conjuncion (x:xs) = if x
                    then conjuncion xs
                    else False

--5. 
disyuncion :: [Bool] -> Bool
disyuncion []= False
disyuncion (x:xs) = if x
                    then True
                    else disyuncion xs


--6. 
aplanar :: [[a]] -> [a]
aplanar []=[]
aplanar (x:xs) =  x ++ aplanar xs 


--7. 
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = if (e == x)
                     then True
                     else pertenece e xs


--8. 
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = if (e ==  x)
                     then 1 + apariciones e xs
                     else apariciones e xs

--9. 
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []= []
losMenoresA n (m:ms)= if (n > m)
                      then m : losMenoresA n ms
                      else losMenoresA n ms


--10.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs


--11.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal x e = x ++ [e]


--12.
agregar :: [a] -> [a] -> [a]
agregar x [] = x
agregar [] y = y 
agregar (x1:xs) y =  x1 : (agregar y xs) 


--13. 
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]


--14. 
zipMaximos :: [Int] -> [Int] -> [Int]
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
zipMaximos [] y = y
zipMaximos x [] = x
zipMaximos (x : xs) (y:ys) = if (x>y)
                            then x : zipMaximos xs ys
                            else y : zipMaximos xs ys 


--15. 
elMinimo :: Ord a => [a] -> a
elMinimo [x]= x
elMinimo (x:xs) = if (x < (elMinimo xs))
                  then x
                  else elMinimo xs
