{-
2. Map (diccionario)
Ejercicio 3
La interfaz del tipo abstracto Map es la siguiente:
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

emptyM = M []

assocM k v (M kvs) = (M (asociar k v kvs))

asociar :: Eq k => k -> v -> [(k, v)] -> [( k, v)]
asociar k v [] = [(k,v)]
asociar k v ((k',v'): kvs) = if k ==k'
                           then (k,v) : kvs
                           else (k',v') : asociar k v kvs


lookupM k (M []) = Nothing
lookupM k (M ((k',v') : kvs)) = if k == k'
                                then Just v'
                                else lookupM k (M kvs)

deleteM _ (M [])  = M []
deleteM k (M kvs) = (M (borrar k kvs))

borrar :: Eq k => k -> [(k, v)] -> [( k, v)]
borrar _ []                 = []
borrar k  ((k', v') : kvs ) = if k == k'
                               then kvs
                               else (k',v') : borrar k kvs

keys (M [])          = []
keys (M ((k,v):kvs)) = k : keys (M kvs) 

