head' :: [a] -> a
head' (x:xs) = x -- constante

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 -- constante

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) -- lineal

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs -- lineal

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs -- cuadratico

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs -- lineal

sinRepetidos :: Eq a => [a] -> [a] -- cuadratico
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs


-- equivalente a (++)
append :: [a] -> [a] -> [a]    -- lineal
append [] ys = ys
append (x:xs) ys = x : append xs ys

concatenar :: [String] -> String  -- lineal
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

takeN :: Int -> [a] -> [a]  -- lineal
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a]   -- lineal
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

partir :: Int -> [a] -> ([a], [a]) -- lineal
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a] -> a -- lineal
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

sacar :: Eq a => a -> [a] -> [a]  --  lineal
sacar n [] = []
sacar n (x:xs) =
                   if n == x
                   then xs
                   else x : sacar n xs

ordenar :: Ord a => [a] -> [a]  -- cuadratica
ordenar [] = []
orderar xs =
            let m = minimo xs
            in m : ordenar (sacar m xs)

agregarACada :: a -> [[a]] -> [[a]] -- lineal
agregarACada x [] = [[x]]
agregarACada x (ys : yss) = (x : ys) : (agregarACada x yss)


zipMaximos :: [Int] -> [Int] -> [Int]
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
zipMaximos [] _ = []
zipMaximos _ [] = []
zipMaximos (x1 : xs) (y1:ys) = if (x1>y1)
                            then x1 : zipMaximos xs ys
                            else y1 : zipMaximos xs ys 


sizeT :: Tree a -> Int
--Dado un árbol binario devuelve su cantidad de elementos
sizeT EmptyT = 0
sizeT (NodeT n t1 t2)= 1 + sizeT t1 + sizeT t2  

perteneceT :: Eq a => a -> Tree a -> Bool
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--árbol.
perteneceT _ EmptyT = False
perteneceT x1 (NodeT x2 t1 t2) = x1 == x2 || perteneceT x1 t1 || perteneceT x1 t2