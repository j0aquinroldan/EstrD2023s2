module Set

(emptyS, addS, belongs, sizeS, removeS, unionS, setToList)

where

data Set = S [a]

emptyS :: Set a
--Crea un conjunto vacío.
emptyS = S []

addS :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
addS x (S xs) = if   belongs x xs 
                then (S xs)
                else (S (x:xs))

belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
belongs x (S xs) = x pertenece xs
                  
sizeS :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
sizeS (S xs) = longitud xs

removeS :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
removeS n [] = []
removeS n (x:xs) = if n == x
                   then xs
                   else x : sacar n xs

unionS :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.


setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.

