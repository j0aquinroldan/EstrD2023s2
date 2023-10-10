import Empresa

-- Ejercicio 2

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show


--1. Costo: O(log N)
belongsBST :: Ord a => a -> Tree a -> Bool
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)
belongsBST x EmptyT = False
belongsBST x (NodeT y ti td) = if x==y 
                               then True
                               else if x<y 
                                     then belongsBST y ti
                                     else belongsBST y td


--2. Costo: O(log N)
insertBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST inserta un elemento en el árbol.
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x == y 
                              then (NodeT x ti td) 
                              else if x<y
                                   then NodeT y (insertBST x ti) td
                                   else NodeT y ti (insertBST x td)


--3.  Costo: O(log N)
deleteBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST borra un elemento en el árbol.
deleteBST x EmptyT = EmptyT
deleteBST x ( NodeT y ti td) = if (x == y)
                               then rearmarBST ti td
                               else if (x < y)
                                    then NodeT y (deleteBST x ti) td 
                                    else NodeT y ti (deleteBST x td) 

rearmarBST :: Ord a => Tree a -> Tree a -> Tree a
rearmarBST EmptyT td = td
rearmarBST ti EmptyT = ti
rearmarBST ti td     = NodeT (maxBST ti) (deleteMaxBST ti) td


maxBST :: Ord a => Tree a -> a
-- PREC no es vacio
maxBST (NodeT x _ EmptyT) = x
maxBST (NodeT _ _ td) = maxBST td

deleteMaxBST :: Ord a => Tree a -> Tree a 
deleteMaxBST (NodeT x _ EmptyT) = EmptyT
deleteMaxBST (NodeT x ti td ) = NodeT x ti (deleteMaxBST td) 



--4. Costo: O(log N) 
-- siendo n la profundidad del tree
splitMinBST :: Ord a => Tree a -> (a, Tree a)
--Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
splitMinBST t = (minBST t , deleteBST (minBST t) t)

-- Costo: O(log N) 
minBST :: Ord a => Tree a -> a
-- PREC no es vacio
minBST (NodeT x EmptyT _) = x
minBST (NodeT _ ti _) = minBST ti


--------------------

--5. Costo: O(log N)
splitMaxBST :: Ord a => Tree a -> (a, Tree a)
--Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
splitMaxBST t = (maxBST t, deleteBST (maxBST t) t )


--6. Costo: O(N2) 
esBST :: Ord a => Tree a -> Bool
--Propósito: indica si el árbol cumple con los invariantes de BST.
esBST EmptyT = True
esBST (NodeT x ti td) = estaEntre x (root ti) (root td) &&  esBST ti && esBST td

root (NodeT x _ _) = x 

estaEntre :: Ord a => a -> a -> a -> Bool
estaEntre x y z = y < x && x < z

-----------------------

--7. Costo: O(log N) 
elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
--elemento dado.
elMaximoMenorA _ EmptyT = Nothing
elMaximoMenorA x (NodeT y ti td) = if (x == y)
                                   then Just (maxBST ti)
                                   else if (x < y )
                                        then elMaximoMenorA x ti
                                        else elMaximoMenorA x ti



--8. Costo: O(log N)
elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
--Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
--elemento dado.
elMinimoMayorA _ EmptyT          = Nothing
elMinimoMayorA x (NodeT y ti td) = if (x==y)
                                   then Just (minBST td)
                                   else if (x < y)
                                        then elMinimoMayorA x ti
                                        else elMinimoMayorA x td


--9. Costo: O(N2)
balanceado :: Tree a -> Bool
--Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
--nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
balanceado EmptyT = True
balanceado (NodeT _ ti td) = abs (heightT ti - heightT td) <= 1

heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
heightT EmptyT = 0
heightT (NodeT x t1 t2) = heightT (elegirRamaMasLarga t1 t2) + 1


elegirRamaMasLarga :: Tree a -> Tree a -> Tree a
elegirRamaMasLarga EmptyT t2 = t2
elegirRamaMasLarga t1 EmptyT  = t1
elegirRamaMasLarga t1 t2 = if heightT t1 > heightT t2
                         then t1 
                         else t2 


{-
Ejercicio 3 
Dada la siguiente interfaz y costos para el tipo abstracto Map:
emptyM :: Map k v
Costo: O(1).
assocM :: Ord k => k -> v -> Map k v -> Map k v
Costo: O(log K).
lookupM :: Ord k => k -> Map k v -> Maybe v
Costo: O(log K).
deleteM :: Ord k => k -> Map k v -> Map k v
Costo: O(log K).
keys :: Map k v -> [k]
Costo: O(K).
recalcular el costo de las funciones como usuario de Map de la práctica anterior, siendo K es la
cantidad de claves del Map. Justicar las respuestas. 

-}


--------------------------------------------------------------------------------------------------

--1.  O(k log k) ya que se hereda de la funcion valoresClaves 
-- y la lista que se recorre con las mismas claves del map
valuesM :: Eq k => Map k v -> [Maybe v]
--Propósito: obtiene los valores asociados a cada clave del map.
valuesM m = valoresClaves (keys m) m 


-- O(n log k) donde k son la cantidad de claves del map y n la cantidad de claves pasadas en la lista,
-- por tanto se recorre la lista y se realiza una operacion logaritmica por cada elemento, lo que resulta
-- en este costo n log k
valoresClaves :: Eq k => [k] -> Map k v -> [Maybe v]
valoresClaves [] _ = []
valoresClaves (k:ks) m  = lookupM k m : valoresClaves ks m

--------------------------------------------------------------------------------------------------

--2. O(n log k) siendo n la longitd de la lista y k la cantidad de claves en el map
-- se recorre la lista realizando una operacion de costo log k sobre cada elemento 
todasAsociadas :: Eq k => [k] -> Map k v -> Bool
--Propósito: indica si en el map se encuentran todas las claves dadas.
todasAsociadas [] m = True
todasAsociadas (k:ks) m = notNothing (lookupM k m ) && todasAsociadas ks m

--O(1)
notNothing ::  Maybe v -> Bool
notNothing Nothing = False
notNothing _ = True 

--------------------------------------------------------------------------------------------------

--3. O(n log k) que es el costo de la operacion asociarlas
listToMap :: Eq k => [(k, v)] -> Map k v
--Propósito: convierte una lista de pares clave valor en un map.
listToMap kvs = asociarlas kvs emptyM

--O(n log k) siendo n la longitd de la lista y k la cantidad de claves en el map
-- se recorre la lista realizando una operacion de costo log k sobre cada elemento 
asociarlas :: Eq k => [(k, v)] -> Map k v -> Map k v 
asociarlas [] m = m
asociarlas ((k,v): kvs) m = asociarlas kvs (assocM k v m )

--------------------------------------------------------------------------------------------------

--4. O(k log k) es el costo de la operacion listar O(n log k) + el costo de keys O(k)
mapToList :: Eq k => Map k v -> [(k, v)]
--Propósito: convierte un map en una lista de pares clave valor.
mapToList m = listar (keys m) m


--O(n log k) siendo n la longitd de la lista y k la cantidad de claves en el map
-- se recorre la lista realizando una operacion de costo log k sobre cada elemento 
listar :: Eq k => [k] -> Map k v-> [(k,v)]
listar [] m = []
listar (k:ks) m = case  lookupM k m of 
       Just x -> (k, x) : listar ks m
       Nothing -> listar ks m

--------------------------------------------------------------------------------------------------

--5. como listToMap es O(n log k) y agruparPorK es O(n^2) entonces el costo total es <<O(n^2)>>
agruparEq :: Eq k => [(k, v)] -> Map k [v]
--Propósito: dada una lista de pares clave valor, agrupa los valores de los pares que compartan 
--la misma clave.
agruparEq kvs = listToMap (agruparPorK kvs)

-- O(n^2) n = long lista
agruparPorK :: Eq k => [(k, v)] -> [(k, [v])]
agruparPorK [] = []
agruparPorK ((k,v):kvs) = (k , v : (valorClave k kvs)) : agruparPorK (sinElemento k kvs)


-- O(n) n = long lista
valorClave :: Eq k => k -> [(k, v)] -> [v]
valorClave k [] = []
valorClave k ((k', v'):kvs) = if k == k'
                              then v' : valorClave k kvs
                              else valorClave k kvs


-- 0(n) n = long de lista
sinElemento :: Eq k => k -> [(k, v)] -> [(k, v)]
sinElemento _ [] = []
sinElemento k ((k',v):kvs) = if k == k'
                             then sinElemento k kvs 
                             else (k',v) : sinElemento k kvs 

----------------------------------------------------------------------------------

--6. PROBLEMA: si la lista esta vacia se vacia el map
--O(n^2): como listToMap y mapToList son O(n log k) y 
-- agregarAPares es O(n^2) entonces el costo total es <<O(n^2)>>
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

----------------------------------------------------------------


--7. O(k log k ) que es el costo de ambas operaciones
mergeMaps:: Eq k => Map k v -> Map k v -> Map k v
--Propósito: dado dos maps se agregan las claves y valores del primer map en el segundo. Si
--una clave del primero existe en el segundo, es reemplazada por la del primero.
mergeMaps m1 m2 = asociarlas (mapToList m1) m2


---------------------------------------------------------------------------------------------

{-
Ejercicio 5
Como usuario del tipo Empresa implementar las siguientes operaciones, 
calculando el costo obtenido al implementarlas, y justicando cada uno adecuadamente.

-}

comenzarCon :: [SectorId] -> [CUIL] -> Empresa
--Propósito: construye una empresa con la información de empleados dada. Los sectores no
--tienen empleados.
--Costo: calcular.
comenzarCon sids cs = agregarEmpleados cs (agregarSectores sids consEmpresa)



agregarSectores :: [SectorId] -> Empresa -> Empresa
agregarSectores [] emp = emp
agregarSectores (sid:sids) emp = agregarSector sid (agregarSectores sids emp)
                               --agregarSectores sid (agregarSector sid emp)

agregarEmpleados :: [CUIL] -> Empresa -> Empresa
-- COSTO : O()
agregarEmpleados [] emp = emp
agregarEmpleados (c:cs) emp= agregarEmpleado [] c (agregarEmpleados cs emp)
 
------------------------------------------------------------------------------------------------------------
recorteDePersonal :: Empresa -> Empresa
--Propósito: dada una empresa elimina a la mitad de sus empleados (sin importar a quiénes).
--Costo: O(n^2)
recorteDePersonal emp = borrarEmpleados (takeN (longitud (todosLosCUIL emp)/2) (todosLosCUIL emp)) emp


borrarEmpleados :: [CUIL] -> Empresa -> Empresa 
borrarEmpleados [] emp = emp
borrarEmpleados (c:cs) emp = borrarEmpleado c (borrarEmpleados cs emp)

takeN :: Int -> [a] -> [a]  -- lineal
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs -- lineal

------------------------------------------------------------------------------------------------------------

convertirEnComodin :: CUIL -> Empresa -> Empresa
--Propósito: dado un CUIL de empleado le asigna todos los sectores de la empresa.
--Costo: o(n) porque absorbe el costo de todoslossectores y agregarempleado es nlogn
convertirEnComodin c emp = agregarEmpleado (todosLosSectores emp) emp

------------------------------------------------------------------------------------------------------------

esComodin :: CUIL -> Empresa -> Bool
--Propósito: dado un CUIL de empleado indica si el empleado está en todos los sectores.
--Costo: O(n^2)
esComodin c emp = esComodin2 c (todosLosSectores emp) emp

esComodin2 :: CUIL -> [SectorId] -> Empresa -> Bool
--costo: O(n^2) siendo n la longitud de la lista
-- hace pertenece que es lineal sobre cada elemento de la lista
esComodin2 c [] emp = True
esComodin2 c (sid:sids) emp = pertenece c (empleadosDelSector sid) && esComodin2 c sids emp