module SetV1

  (Set , emptySet, addSet, belongs, sizeSet, removeSet, unionSet, setToList)
where

data Set a = Set [a] Int 
            -- lista, cantidad elementos

emptySet :: Set a
--Crea un conjunto vacío.
addSet :: Eq a => a -> Set a -> Set a
--Dados un elemento y un conjunto, agrega el elemento al conjunto.
belongs :: Eq a => a -> Set a -> Bool
--Dados un elemento y un conjunto indica si el elemento pertenece al conjunto.
sizeSet :: Eq a => Set a -> Int
--Devuelve la cantidad de elementos distintos de un conjunto.
removeSet :: Eq a => a -> Set a -> Set a
--Borra un elemento del conjunto.
unionSet :: Eq a => Set a -> Set a -> Set a
--Dados dos conjuntos devuelve un conjunto con todos los elementos de ambos. conjuntos.
setToList :: Eq a => Set a -> [a]
--Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.




emptySet = Set [] 0

addSet x (Set xs l) = if   pertenece x xs 
                  then (Set xs l)
                  else (Set (x:xs) (l+1))

belongs x (Set xs _) = pertenece x  xs

sizeSet (Set _ l) = l

removeSet x (Set ys l) = Set (sacar x ys) (longitud ((sacar x ys)))

unionSet s1 (Set [] _)  = s1
unionSet s1 (Set (y:ys) l2) = unionSet (addSet y s1) (Set ys l2)

setToList (Set xs _) = xs


--------------------------------------
-- ===================================
-------------------------------------- 

sacar :: Eq a => a -> [a] -> [a] 
sacar x   []   = []
sacar x (y:ys) = if (x == y)
                 then ys
                  else y : sacar x ys

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs



longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs