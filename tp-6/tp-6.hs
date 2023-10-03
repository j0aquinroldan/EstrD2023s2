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

ejMap :: Map Char Int
ejMap =  assocM 'a' 4  $
       assocM 'b' 3  $
       assocM 'c' 2  $
       assocM 'd' 1 emptyM

ejMap2 :: Map Char Int
ejMap2 = assocM 'a' 5  $
       assocM 'b' 1  $
       assocM 'e' 2  $
       assocM 'f' 1 emptyM

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



--4.  O(n^2): n siendo la cantidad de elementos que haya en el map
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

--6. PROBLEMA: si la lista esta vacia se vacia el map
--O(n^2):
incrementar :: Eq k => [k] -> Map k Int -> Map k Int
--Propósito: dada una lista de claves de tipo k y un map que va de k a Int, le suma uno a
--cada número asociado con dichas claves.
incrementar ks m = listToMap (agregarAPares ks (mapToList m))


--O(n^2): n siendo la cantidad de elementos de la primera lista
agregarAPares :: Eq k => [k] -> [(k, Int)] -> [(k, Int)]
--Propósito: dada una lista de claves de tipo k y otra lista de tuplas k a Int, le suma uno a
--cada número asociado con dichas claves.
agregarAPares [] kns = []
agregarAPares (k:ks) kns = agregarAPares2 k kns ++ agregarAPares ks kns


--O(n): n siendo la cantidad de elementos de la lista
agregarAPares2 :: Eq k => k -> [(k, Int)] -> [(k, Int)]
--Propósito: dada una clave de tipo k y una lista de tuplas k a Int, le suma uno al
-- número asociado con dicha clave, al que no esta asociado no lo agrega
agregarAPares2 _  []  = []
agregarAPares2 k ((k',n):kns) = if k == k'
                                    then (k',n+1) :  kns
                                    else (k',n) : agregarAPares2 k kns

---------------


--7. 
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps m1 m2 = asociarlas (mapToList m1) m2


-- ====================================================================

--Ejercicio 5
--Implemente estas otras funciones como usuario de Map:

-- 0(n^2) donde n es la cantidad de elementos en la lista
indexar :: [a] -> Map Int a
--Propósito: dada una lista de elementos construye un map que relaciona cada elemento con
--su posición en la lista.
indexar xs = indexarMap xs 0 emptyM

-- 0(n^2) donde n es la cantidad de elementos en la lista
indexarMap :: [a] -> Int -> Map Int a -> Map Int a
indexarMap [] _ m = m
indexarMap (x:xs) n m = assocM n x (indexarMap xs (n+1) m) 

----------------------------------------

--O(n^2) 
-- n siendo la cantidad de caracteres en el string
ocurrencias :: String -> Map Char Int
--Propósito: dado un string, devuelve un map donde las claves son los caracteres que aparecen
--en el string, y los valores la cantidad de veces que aparecen en el mismo.
ocurrencias s = ocurrenciasIndex (sinRepetidos s) s emptyM

--O(n^2)  O(n^2 x m)????????? m siendo la cantidad de elementos en el segundo strign
-- n siendo la cantidad de caracteres en el primer string
ocurrenciasIndex :: String -> String -> Map Char Int -> Map Char Int 
ocurrenciasIndex [] _ m = m
ocurrenciasIndex (s1:ss1) ss2 m = assocM s1 (apariciones s1 ss2) (ocurrenciasIndex ss1 ss2 m)  


-- O(n)
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = if (e ==  x)
                     then 1 + apariciones e xs
                     else apariciones e xs

-- O(n^2)
sinRepetidos :: Eq a => [a] -> [a] 
sinRepetidos [] = []
sinRepetidos (x:xs) = if elem x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs




{-


1. Implementar el tipo abstracto MultiSet utilizando como representación un Map. Indicar los
ordenes de complejidad en peor caso de cada función de la interfaz, justicando las respuestas.
2. Reimplementar como usuario de MultiSet la función ocurrencias de ejercicios anteriores,
que dado un string cuenta la cantidad de ocurrencias de cada caracter en el string. En este
caso el resultado será un multiconjunto de caracteres.

-}