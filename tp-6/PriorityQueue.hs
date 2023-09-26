{-
1. Priority Queue (cola de prioridad)
Ejercicio 1
La siguiente interfaz representa colas de prioridad, llamadas priority queue, en inglés. La misma
posee operaciones para insertar elementos, y obtener y borrar el mínimo elemento de la estructura.
Implementarla usando listas, e indicando el costo de cada operación.

-}
module PriorityQueue

(PriorityQueue, emptyPQ, isEmptyPQ, insertPQ, findMinPQ, deleteMinPQ)

where

data PriorityQueue a = PQ [a]    


emptyPQ :: PriorityQueue a                                                      --O(1)
--PROP: devuelve una priority queue vacía.
isEmptyPQ :: PriorityQueue a -> Bool                                            --O(1)
--PROP: indica si la priority queue está vacía.
insertPQ :: Ord a => a -> PriorityQueue a -> PriorityQueue a                    --O(1)
--PROP: inserta un elemento en la priority queue.
findMinPQ :: Ord a => PriorityQueue a -> a                                      --O(n)
--PROP: devuelve el elemento más prioriotario (el mínimo) de la priority queue.
--PREC: parcial en caso de priority queue vacía.
deleteMinPQ :: Ord a => PriorityQueue a -> PriorityQueue a                      --O(n)
--PROP: devuelve una priority queue sin el elemento más prioritario (el mínimo).
--PREC: parcial en caso de priority queue vacía.


emptyPQ = PQ []

isEmptyPQ (PQ xs) = null xs 

insertPQ x (PQ xs) = (PQ (x:xs))

findMinPQ (PQ xs) = minimum xs

deleteMinPQ (PQ xs) = PQ (borrarMin xs)

borrarMin :: Ord a => [a] -> [a]

borrarMin xs = borrar (minimum xs) xs

borrar :: Eq a => a -> [a] -> [a]
borrar x [] = []
borrar x (y:ys) = if x == y 
                   then ys
                   else y : borrar x ys

