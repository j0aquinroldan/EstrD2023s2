{-
3. Como dos listas, una de claves y otra de valores, donde la clave ubicada en la posición i está
asociada al valor en la misma posición, pero de la otra lista.
-}


module Map

(Map, emptyM, assocM, lookupM, deleteM, keys) 
where

data Map k v = M [k] [v] 

{-
INV REP: 
 - en M ks vs:
   - ks representa las claves y vs los valores, ambas listas tiene la misma longitud
-}


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
emptyM = M [] []

--O(n)
assocM k v (M ks vs) = (M (k:ks) (v:vs))


--O(n)
lookupM k (M [] _) = Nothing
lookupM k (M  (k:ks) vs) = 


getIndex :: Eq a => a -> [a] -> Int
--Prec x esta en la lista
getIndex x _ = 0
getIndex x (x':xs) = unoSi (x==x') + getIndex x xs  

--O(n)
deleteM k (M kvs) = (M (borrar k kvs))

--O(n)
borrar :: Eq k => k -> [(k, v)] -> [( k, v)]
borrar _ []                 = []
borrar k  ((k', v') : kvs ) = if k == k'
                               then kvs
                               else (k',v') : borrar k kvs


--O(1)
keys (M [] _) = []
keys (M ks _) = ks 

