import PriorityQueue
import Map

ejPQ = insertPQ 10 
       $ insertPQ 5
       $ insertPQ 8
       $ insertPQ 4 
       $ emptyPQ

instance (Show a, Ord a) => Show (PriorityQueue a) where
  show pq = if isEmptyPQ pq 
             then " ||"
             else " << " ++ show (findMinPQ pq) ++ show (deleteMinPQ pq)

{-
Ejercicio 2
Implementar la función heapSort :: Ord a => [a] -> [a], que dada una lista la ordena de
menor a mayor utilizando una Priority Queue como estructura auxiliar. ¾Cuál es su costo?
-}

--O(n^2)
heapSort :: Ord a => [a] -> [a]
heapSort xs = toListPQ (insertAllPQ xs emptyPQ)


-- O(n)
insertAllPQ ::  Ord a => [a] -> PriorityQueue a-> PriorityQueue a
insertAllPQ [] pq = pq 
insertAllPQ (x:xs) pq = insertAllPQ xs ( insertPQ x pq )

--O(n^2)
toListPQ :: Ord a => PriorityQueue a -> [a]
toListPQ pq = if isEmptyPQ pq
              then []
              else findMinPQ pq : toListPQ (deleteMinPQ pq)


{-

Implementar como usuario del tipo abstracto Map las siguientes funciones:
1. valuesM :: Eq k => Map k v -> [Maybe v]
Propósito: obtiene los valores asociados a cada clave del map.
2. todasAsociadas :: Eq k => [k] -> Map k v -> Bool
Propósito: indica si en el map se encuentran todas las claves dadas.
3. listToMap :: Eq k => [(k, v)] -> Map k v
Propósito: convierte una lista de pares clave valor en un map.
4. mapToList :: Eq k => Map k v -> [(k, v)]
Propósito: convierte un map en una lista de pares clave valor.
5. agruparEq :: Eq k => [(k, v)] -> Map k [v]
Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan
la misma clave.
6. incrementar :: Eq k => [k] -> Map k Int -> Map k Int
Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
cada número asociado con dichas claves.
7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
una clave del primero existe en el segundo, es reemplazada por la del primero.
-}