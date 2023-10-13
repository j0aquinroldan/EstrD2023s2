import Organizador 


--a) Implementar las siguientes funciones como usuario del TAD Organizador, establecer su eficiencia y justificarla:
--a) 

-- USEEER
-- O(
--   log P + log P # por programasDe 
--   c log c ) # por intersection sobre sets de cheksums
-- O(log P + c log c )
 
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
--programaron juntas.
programaronJuntas p1 p2 o = intersection (programasDe p1 o) (programasDe p2 o)

-----------------------------------

--b)
esUnGranHacker :: Organizador -> Persona -> Bool
--Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador
-- COSTO: 
--longitud = O(C), donde C es la cantidad de elementos en la lista.
--todosLosProgramas = O(C), donde C es la cantidad de códigos en el organizador.
-- sizeS = O(1)
-- programasDe= O(log P), donde P es la cantidad total de personas del organizador.
-- O(C) + O(C) + O(log P) => como los costos lineales tienen mas peso =>
-- O(C) + O(C) => 2 O(C) =>
-- EFICIENCIA = O(C) 
esUnGranHacker o p = longitud (todosLosProgramas o) == sizeS (programasDe p o)

---------------------------------------------------------------------------------------------------------------------------------



{-







-}
