--1. Cálculo de costos

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

partir :: Int -> [a] -> ([a], [a]) -- cuadratica
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a] -> a -- cuadratica
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






{-
2. Set (conjunto)
Un Set es un tipo abstracto de datos que consta de las siguientes operaciones:
emptyS :: Set a
Crea un conjunto vacío.
addS :: Eq a => a -> Set a -> Set a
Dados un elemento y un conjunto, agrega el elemento al conjunto.
belongs :: Eq a => a -> Set a -> Bool
Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
sizeS :: Eq a => Set a -> Int
Devuelve la cantidad de elementos distintos de un conjunto.
removeS :: Eq a => a -> Set a -> Set a
Borra un elemento del conjunto.
unionS :: Eq a => Set a -> Set a -> Set a
Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList :: Eq a => Set a -> [a]
Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.


1. Implementar la variante del tipo abstracto Set con una lista que no tiene repetidos y guarda
la cantidad de elementos en la estructura.
Nota: la restricción Eq aparece en toda la interfaz se utilice o no en todas las operaciones
de esta implementación, pero para mantener una interfaz común entre distintas posibles
implementaciones estamos obligados a escribir así los tipos.
2. Como usuario del tipo abstracto Set implementar las siguientes funciones:
losQuePertenecen :: Eq a => [a] -> Set a -> [a]
Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
al conjunto.
sinRepetidos :: Eq a => [a] -> [a]
Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
unirTodos :: Eq a => Tree (Set a) -> Set a
Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
del arbol.
3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
por ejemplo). Contrastar la eciencia obtenida en esta implementación con la anterior.



-}