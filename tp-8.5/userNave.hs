import Nave

--a) Implementar las siguientes funciones como usuario del TAD N ave, establecer su eficiencia y justificarla:

--a) 
tripulantes :: Nave -> Set Tripulante
--Propósito: Denota los tripulantes de la nave
-- CSOTO: 
-- tripulantesDeLos O() 
-- sectores O(S) 
-- siendo T la cantidad de tripulantes en la nave y siendo S la cantidad de sectores,
-- O(S + S * (T log T + log S)) => O(S * (T log T + log S))

tripulantes n = tripulantesDeLos (sectores n) n

tripulantesDeLos :: [Sector] -> Nave -> Set Tripulante
-- PROP: devuelve los tripulantes de los sectores dados en la nave
--COSTO :
-- recursion sobre sectores O(S) siendo S la longitud de la lista
-- tripulantesDe  O(log S) siendo S la cantidad de sectores en la nave
-- estas dos S coinciden ya que en peor caso la lista contiene todos los sectores de la nave
-- unionS O(T log T) siendo T la cantidad de tripulantes en el set
-- emptyS O(1)
-- O(S * (T log T + log S))
tripulantesDeLos [] _ = emptyS 
tripulantesDeLos (s:ss) n = unionS (tripulantesDe s n) (tripulantesDeLos ss n)

--------------------------------------------------------------------------------------------------------


--b) Opcional (Bonus): 
bajaDeTripulante :: Tripulante -> Nave -> Nave
--Propósito: Elimina al tripulante de la nave.
--Pista: Considere reconstruir la nave sin ese tripulante.

--------------------------------------------------------------------------------------------------------

