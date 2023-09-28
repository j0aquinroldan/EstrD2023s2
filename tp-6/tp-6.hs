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


ejMap =  assocM 'a' 4  $
       assocM 'b' 3  $
       assocM 'c' 2  $
       assocM 'd' 1 emptyM

--1.  O(n^2)
valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valoresClaves (keys m) m 


-- O(n^2)
valoresClaves :: Eq k => [k] -> Map k v -> [Maybe v]
valoresClaves [] _ = []
valoresClaves (k:ks) m  = lookupM k m : valoresClaves ks m


--2. O(n^2)
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas [] m = True
todasAsociadas (k:ks) m = notNothing (lookupM k m ) && todasAsociadas ks m

--O(1)
notNothing ::  Maybe v -> Bool
notNothing Nothing = False
notNothing _ = True 


--3. --O(n^2)
listToMap :: Eq k => [(k, v)] -> Map k v
--Propósito: convierte una lista de pares clave valor en un map.
listToMap kvs = asociarlas kvs emptyM

--O(n^2)
asociarlas :: Eq k => [(k, v)] -> Map k v -> Map k v 
asociarlas [] m = m
asociarlas ((k,v): kvs) m = asociarlas kvs (assocM k v m )



--4.  ()
mapToList :: Eq k => Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor.
mapToList m = listar (keys m) m


--O(n^2)
listar :: Eq k => [k] -> Map k v-> [(k,v)]
listar [] m = []
listar (k:ks) m = (k, valor (lookupM k m)) : listar ks m

valor :: Maybe a -> a
valor (Just v) = v



--5. 
agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan 
--la misma clave.
agruparEq kvs = listToMap (agruparPorK kvs)

agruparPorK :: Eq k => [(k, v)] -> [(k, [v])]
agruparPorK [] = []
agruparPorK ((k,v):kvs) = (k , [v] ++ valorClave k kvs) : agruparPorK (sinElemento k kvs)

valorClave :: Eq k => k -> [(k, v)] -> [v]
valorClave k [] = []
valorClave k ((k', v'):kvs) = if k == k'
                              then v' : valorClave k kvs
                              else valorClave k kvs

sinElemento :: Eq k => k -> [(k, v)] -> [(k, v)]
sinElemento _ [] = []
sinElemento k ((k',v):kvs) = if k == k'
                             then sinElemento k kvs 
                             else (k',v) : sinElemento k kvs 

---------------------------------
--6. 
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves.

incrementar ks m = agregarAPares ks (mapToList m)


-- SE ESTA HACIENDO RECURSION SOBRE LAS DOS LISTAS, CORREGIR CON DOS FUNCIONES EN LAS QUE CADA UNA RECPORRA C
-- CADA LISTA
agregarAPares :: Eq k => [k] -> [(k, Int)] -> [(k, Int)]

agregarAPares [] kns = kns
agregarAPares _  []  = []
agregarAPares (k:ks) ((k',n):kns) = if k == k'
                                    then (k',n+1) : agregarAPares k kns
                                    else (k',n) : agregarAPares k kns

{-
assocM k (valor (lookupM k m) + 1) m 
Implementar como usuario del tipo abstracto Map las siguientes funciones:





7. mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
una clave del primero existe en el segundo, es reemplazada por la del primero.
-}