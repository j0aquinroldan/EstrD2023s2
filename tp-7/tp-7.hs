
-- Ejercicio 2

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show


--1. Costo: O(log N)
belongsBST :: Ord a => a -> Tree a -> Bool
--Propósito: dado un BST dice si el elemento pertenece o no al árbol.
--Costo: O(log N)
belongsBST x EmptyT = 
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
deleteBST x ( NodeT y )



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



4. splitMinBST :: Ord a => Tree a -> (a, Tree a)
Propósito: dado un BST devuelve un par con el mínimo elemento y el árbol sin el mismo.
Costo: O(log N)
5. splitMaxBST :: Ord a => Tree a -> (a, Tree a)
Propósito: dado un BST devuelve un par con el máximo elemento y el árbol sin el mismo.
Costo: O(log N)
6. esBST :: Tree a -> Bool
Propósito: indica si el árbol cumple con los invariantes de BST.
Costo: O(N2
)
7. elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
Propósito: dado un BST y un elemento, devuelve el máximo elemento que sea menor al
elemento dado.
Costo: O(log N)
8. elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
Propósito: dado un BST y un elemento, devuelve el mínimo elemento que sea mayor al
elemento dado.
Costo: O(log N)
9. balanceado :: Tree a -> Bool
Propósito: indica si el árbol está balanceado. Un árbol está balanceado cuando para cada
nodo la diferencia de alturas entre el subarbol izquierdo y el derecho es menor o igual a 1.
Costo: O(N2
)

-}