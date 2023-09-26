{-3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
por ejemplo). Contrastar la eficiencia obtenida en esta implementación con la anterior.-}

module SetV2

(Set , emptySet, addSet, belongs, sizeSet, removeSet, unionSet, setToList)

where


data Set a = Set [a] Int 
            -- lista, cantidad elementos

{-
INV REP:
    Set xs l 
    l representa la longitud de xs
-}

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

addSet x (Set xs l) = Set (x:xs) (l+1)

{-
addSet x (Set xs l) = if   pertenece x xs 
                  then (Set (x:xs) l)
                  else (Set (x:xs) (l+1))
-}

belongs x (Set xs _) = pertenece x  xs

sizeSet (Set xs _ ) = longitud (sinRepetidos xs)

{-
sizeS (Set _ l) = l
-}

removeSet x (Set ys l) = Set (sacarTodos x ys) (longitud ((sacarTodos x ys)))

unionSet s1 (Set [] _)  = s1
unionSet s1 (Set (y:ys) l2) = unionSet (addSet y s1) (Set ys l2)

setToList (Set xs _) = sinRepetidos xs


sacarTodos :: Eq a => a -> [a] -> [a] 
-- PROP: saca el elemento todas las veces que este repetido
sacarTodos x   []   = []
sacarTodos x (y:ys) = if (x == y)
                 then sacarTodos x ys
                  else y : sacarTodos x ys

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs



sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                    then sinRepetidos xs
                    else x : sinRepetidos xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs