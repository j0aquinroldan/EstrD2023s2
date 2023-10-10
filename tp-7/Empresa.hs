
module Empresa

{-
INV REP : 
    -en el segundo map cada clave esta asociada a un valor empleado con dicha clave como cuil.
    - todo sectorId asociado a un empleado ya sea parte de un conjunto valor del primer map o valor del segundo map 
        tiene que estar en el primer map como clave.
    - todo sector id del primer map tiene que tener asociado un conjunto con dichos empleados que tiene dicho sector asociado
    - todo empleado que conforma algun set valor del primer map tiene que se valor del segundo map. 
    -
-}

(Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores,agregarSector,
agregarEmpleado, agregarASector)
where 

import Empleado
import Map
import SetV1

type SectorId = Int
type CUIL = Int
data Empresa = ConsE (Map SectorId (Set Empleado)) (Map CUIL Empleado)


consEmpresa :: Empresa
--Propósito: construye una empresa vacía.
--Costo: O(1) es el costo de emptyM
consEmpresa = ConsE emptyM emptyM

----------------------------------------------------------------------------------------------------

buscarPorCUIL :: CUIL -> Empresa -> Empleado
--Propósito: devuelve el empleado con dicho CUIL.
--Costo: O(log E) es el costo de hacer lookupM sobre el map de cuils y empleados
buscarPorCUIL c (ConsE _ mc) = case lookupM c mc of
                               Just e -> e
                               Nothing -> error "el empleado no esta registrado en esta empresa"


----------------------------------------------------------------------------------------------------

empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Propósito: indica los empleados que trabajan en un sector dado.
--Costo: O(logS + E) ????
empleadosDelSector sId (ConsE ms _ ) = case valor(lookupM sId ms) of
                                       Just s -> (setToList s)
                                       Nothing -> error "no existe dicho sector en la empresa"

----------------------------------------------------------------------------------------------------

todosLosCUIL :: Empresa -> [CUIL]
--Propósito: indica todos los CUIL de empleados de la empresa.
--Costo: O(E) absorbe el costo de keys del modulo map que es O(K) siendo k la cantidad de claves,
-- en este caso la cantidad de empleados.
todosLosCUIL (ConsE _ mc) = keys mc

----------------------------------------------------------------------------------------------------

todosLosSectores :: Empresa -> [SectorId]
--Propósito: indica todos los sectores de la empresa.
--Costo: O(S) absorbe el costo de keys del modulo map que es O(K) siendo k la cantidad de claves, 
-- en este caso la cantidad de sectores.
todosLosSectores (ConsE ms _) = keys ms
----------------------------------------------------------------------------------------------------

agregarSector :: SectorId -> Empresa -> Empresa
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
--Costo: O(logS) absorbe el costo de assocM que es O(logK) siendo k la cantidad de sectores en el map
-- mientras que emptySet es una operacion constante por tanto no tiene influencia
agregarSector sId (ConsE ms mc) = ConsE (assocM sId emptySet ms) mc
----------------------------------------------------------------------------------------------------

agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Propósito: agrega un empleado a la empresa, en el que trabajará en dichos sectores y tendrá
--el CUIL dado.
--PREC : los secotres estan en la empresa
-- costo: O(log n)
agregarEmpleado sids c (ConsE ms mc) = let empleado = incorporarSectores sids (consEmpleado c) in
                                       ConsE (agregarASectores sids empleado ms) (assocM c empleado mc) 


agregarASectores :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
-- PROP: agrega el empleado a todos los sectores del map que esten en la lista
--PREC : los sectores estan entre las claves del map
-- costo: O(n log n)
agregarASectores [] _ ms = ms
agregarASectores (sid:sids) emp ms = agregarASector2 sid emp (agregarASectores sids emp )

agregarASector2 :: SectorId -> Empleado -> (Map SectorId (Set Empleado)) -> (Map SectorId (Set Empleado))
--PROP : agrega el empleado al sector del map que coincida con el dado por parametro
--PREC : el sector esta entre las claves del map
-- costo: O(log n)
agregarASector2 sid emp ms = assocM sid (addSet emp (valor(lookupM sid ms ))) ms
--                    asoc sector (-agrega emp-da el set-busca el sector) al map

valor :: Maybe a -> a
valor (Just v) = v

incorporarSectores :: [SectorId] -> Empleado -> Empleado
--PROP : incorpora todos los sectores de la lista en el empleado O(nlog n)
incorporarSectores [] e = e
incorporarSectores (sid:sids) e = incorporarSectores sids (incorporarSector sid e)

{-
-- O(n) n = long sectores
incorporarSectores :: [SectorId] -> Empleado -> Empleado
incorporarSectores [] e = e
incorporarSectores (sid:sids) e = incorporarSector sid (incorporarSectores sids e) 
-}
----------------------------------------------------------------------------------------------------

agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: addSet O(log n) + assocM O(log n) + lookupM O(log n) = agregarASector O(log n)
agregarASector sid c (ConsE ms mc) = case (lookupM c mc) of 
                                     Just s -> ConsE ms (assocM c (addSet sid s) mc)
                                     Nothing -> error "no existe el empleado"

-----------------------------------------------------------------------------

borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito: elimina al empleado que posee dicho CUIL.
--COSTO : O(n^2) 
borrarEmpleado c (ConsE ms mc) = let secEmpleado = sectores (lookupM c mc) in 
                                 ConsE (borrarADeLos c secEmpleado ms) (deleteM c mc) 


borrarADeLos :: CUIL -> [SectorId] -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
--PROP: borra el empleado con el cuil dado de los sectores dados
--COSTO : O(n^2) 
borrarADeLos c [] ms = ms
borrarADeLos c (sid:sids) ms = borrarADeLos c sids (borrarADe c sid ms)

borrarADe :: CUIL -> SectorId -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)
--PROP: borra el empleado con el cuil indicado del sector dado en el map
--COSTO : O(n)
borrarADe c sid ms = case lookupM sid ms of                       -- busca el sector y retorna set empleados 
                     Just se -> assocM sid (agregarTodos(eliminarEmpleado c se)) ms
                    -- asocia el sector y el set emp sin el empleado del cuil dado al map
                     Nothing -> error "el sector no existe"
                     

eliminarEmpleado :: CUIL ->  [Empleado] -> Set Empleado
--PROP : elimina el empleado con el cuil dado de la lista de empleados
--PREC: la lista no tiene repetidos
--COSTO : O(n)
eliminarEmpleado _ [] = emptySet
eliminarEmpleado c (e:es) = if c == (cuil e)
                            then es
                            else e : (eliminarEmpleado c es)

agregarTodos :: Eq a => [a] -> Set a -> Set a
--PROP: agrega todos los elementos de la list a un set
--COSTO : O(n)
agregarTodos [] s = s 
agregarTodos (x:xs) s = agregarTodos xs (addSet x s)



