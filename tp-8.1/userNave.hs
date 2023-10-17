import Nave 

--i)
sectores :: Nave -> Set SectorId
--Propósito: Devuelve todos los sectores no vacíos (con tripulantes asignados).
-- COSTO: 
-- sectoresDeLos O(T * (S log S))
-- tripulantesN O(log T)
-- O( T * (S log S) + log T) =>  O( T * (S log S))
sectores n = sectoresDeLos (tripulantesN n) 

sectoresDeLos :: [Tripulante] -> Set SectorId
--PROP : devuelve los sectores de cada tripulante
-- COSTO: 
-- recursion O(T), siendo T la longitud de la lista de tripulantes
-- unionS O( S log S), siendo S la cantidad de SectorId en el set
-- sectoresT O(1)
-- emptyS O(1)
-- O(T * ((S log S) + 1) + 1) =>  O (T * (S log S))
sectoresDeLos [] = emptyS 
sectoresDeLos (t:ts) n =  unionS (sectoresT t) (sectoresDeLos ts) 

------------------------------------------------------------------------------------------------------

--j) 
sinSectoresAsignados :: Nave ->[Tripulante]
--Propósito: Devuelve los tripulantes que no poseen sectores asignados.
-- COSTO:
-- losSinSectores O(T), siendo T la cantidad de tripulantes en la nave
-- tripulantesN O (log T)
-- O(T + log T) => O(T)
sinSectoresAsignados n = losSinSectores (tripulantesN)

losSinSectores :: [Tripulante] -> [Tripulante]
-- PROP: devuelve aquellos tripulantes de la lista dada que no tengan sectores asignados
-- COSTO: 
-- recursion sobre [Tripulante] -> O(T), siendo T la longitud de la lista
-- sectoresT                       O(1)
-- sizeS                           O(1)
--                             -------------
--                            O(T * (1+1)) => O(T)
losSinSectores [] = []
losSinSectores (t:ts) = if (sizeS (sectoresT t) == 0)
                        then t : losSinSectores ts
                        else losSinSectores ts


--k) 
barriles :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de los sectores asignados de la nave.
barriles n =  barrilesSectores (set2List (sectores n )) n

barrilesSectores :: [SectorId] -> Nave -> [Barril]
--PROP : devuelve los barriles del componente
-- COSTO: 
-- recursion sobre [SectorId] O(S), siendo S la longitud de la lista
-- datosDeSector O(log S), siendo S la cantidad de sectores en la nave (coinciden con la longitud de la lista de arriba)
-- barrilesComponentes O(C), siendo C la cantidad de componentes por sector
-- O( S * (log S + C)) => O(S * C)
barrilesSectores [] n = []
barrilesSectores (sid:sids) n = let (ts,cs) = (datosDeSector sid n) in 
                                (barrilesComponentes cs) ++ (barrilesSectores sids n)  

barrilesComponentes :: [Componente] -> [Barriles]
--PROP : devuelve los barriles de cada componente en la lista dada
-- COSTO : O(C), siendo C la longitud de la lista
barrilesComponentes [] = []
barrilesComponentes (c:cs) = barrilesC c : barrilesComponentes cs

barrilesC :: Componente -> [Barriles]
--PROP : devuelve los barriles del componente
-- COSTO : O(1)
barrilesC Almacen bs = bs
barrilesC _ = []

datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])


{-
Usuario
Implementar las siguientes funciones como usuario del tipo Nave, indicando la eficiencia obtenida para cada operación:



Bonus
l) Dar una posible representación para el tipo Sector, de manera de que se pueda cumplir con el orden dado para cada
operación de la interfaz, pero sin implementarlas.
-}