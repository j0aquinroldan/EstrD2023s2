module Empleado

(Empleado, consEmpleado, cuil, incorporarSector, sectores)
where 

type SectorId = Int
type CUIL = Int
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

data Empleado = ConsEmp CUIL [SectorId]

consEmpleado :: CUIL -> Empleado
--Propósito: construye un empleado con dicho CUIL.
--Costo: O(1)
consEmpleado c = ConsEmp c []

cuil :: Empleado -> CUIL
--Propósito: indica el CUIL de un empleado.
--Costo: O(1)
cuil (ConsEmp c _) = c

incorporarSector :: SectorId -> Empleado -> Empleado
--Propósito: incorpora un sector al conjunto de sectores en los que trabaja un empleado.
--Costo: O(log S), siendo S la cantidad de sectores que el empleado tiene asignados.
incorporarSector sid (ConsEmp c sids) = ConsEmp c (sid : sids)

sectores :: Empleado -> [SectorId]
--Propósito: indica los sectores en los que el empleado trabaja.
--Costo: O(S) ??????????
sectores (ConsEmp _ sids) = sids 
