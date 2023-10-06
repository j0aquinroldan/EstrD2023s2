
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
Ejercicio 1
Indicar el costo de heapsort :: Ord a => [a] -> [a] (de la práctica anterior) suponiendo que
el usuario utiliza una priority queue con costos logarítmicos de inserción y borrado (o sea, usa una
Heap como tipo de representación).


Ejercicio 2
Implementar las siguientes funciones suponiendo que reciben un árbol binario que cumple los
invariantes de BST y sin elementos repetidos (despreocuparse por el hecho de que el árbol puede
desbalancearse al insertar o borrar elementos). En todos los costos, N es la cantidad de elementos
del árbol. Justicar por qué la implementación satisface los costos dados.









-}