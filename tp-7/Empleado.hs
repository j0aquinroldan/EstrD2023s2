module Empleado

(Empleado, consEmpleado, cuil, incorporarSector, sectores)
where 

type SectorId = Int
type CUIL = Int
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

data Empleado = ConsEmp CUIL (Tree SectorId)

consEmpleado :: CUIL -> Empleado
--Propósito: construye un empleado con dicho CUIL.
--Costo: O(1)
consEmpleado c = ConsEmp c EmptyT

cuil :: Empleado -> CUIL
--Propósito: indica el CUIL de un empleado.
--Costo: O(1)
cuil (ConsEmp c _) = c

incorporarSector :: SectorId -> Empleado -> Empleado
--Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
--Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
incorporarSector sid (ConsEmp c sids) = ConsEmp c (insertBST sid sids)

sectores :: Empleado -> [SectorId]
--Propósito: indica los sectores en los que el empleado trabaja.
--Costo: O(S) 
sectores (ConsEmp _ sids) = inorder sids 


insertBST :: Ord a => a -> Tree a -> Tree a
--Propósito: dado un BST inserta un elemento en el árbol.
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT y ti td) = if x == y 
                              then (NodeT x ti td) 
                              else if x<y
                                   then NodeT y (insertBST x ti) td
                                   else NodeT y ti (insertBST x td)



inorder :: Tree a -> [a]    -- O(n^2), si (++) es O(n)
                            -- pero O(n), si (++) es O(1)
inorder EmptyT          = []
inorder (NodeT x ti td) = inorder ti ++ [x] ++ inorder td