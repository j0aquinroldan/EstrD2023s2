
-- O(n) : por costo de O (assocM k v) + costo de O(lookupM k  v)
agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan 
--la misma clave.
agruparEq [] = emptyM
agruparEq ((k,v):xs) = case lookupM k agruparEq xs of
    Just ys -> assocM k (v:ys) (agruparEq xs) 
    Nothing -> assocM k [v] (agruparEq xs) 