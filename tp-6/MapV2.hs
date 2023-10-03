{-
Ejercicio 4
Implemente las siguientes variantes del tipo Map, indicando los costos obtenidos para cada operación, justicando las respuestas:
2. Como una lista de pares-clave valor con claves repetidas

-}


module Map

(Map, emptyM, assocM, lookupM, deleteM, keys) 
where

data Map k v = M [(k,v)]


emptyM :: Map k v
--Propósito: devuelve un map vacío
assocM :: Eq k => k -> v -> Map k v -> Map k v
--Propósito: agrega una asociación clave-valor al map.
lookupM :: Eq k => k -> Map k v -> Maybe v
--Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v
--Propósito: borra una asociación dada una clave.
keys :: Map k v -> [k]
--Propósito: devuelve las claves del map.


--O(1)
emptyM = M []

--O(1)
assocM k v (M kvs) = (M ((k, v) : kvs))

--O(n)
lookupM k (M []) = Nothing
lookupM k (M ((k',v') : kvs)) = if k == k'
                                then Just v'
                                else lookupM k (M kvs)

--O(n)
deleteM k (M kvs) = (M (borrar k kvs))

--O(n)
borrar :: Eq k => k -> [(k, v)] -> [( k, v)]
borrar _ []                 = []
borrar k  ((k', v') : kvs ) = if k == k'
                               then borrar k kvs
                               else (k',v') : borrar k kvs


--O(n^2)
keys (M [])          = []
keys (M ((k,v):kvs)) = sinRepetidos (k : keys (M kvs)) 

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                    then sinRepetidos xs
                    else x : sinRepetidos xs