--b) Implementar el TAD N ave suponiendo el siguiente tipo de representación:

--Donde:
--Cada tripulante puede estar en un sector como máximo.
--Se guarda al sector con más tripulantes de la nave y cuántos tripulantes tiene ese sector.
--Los tripulantes se ordenan por rango de mayor a menor en la Heap
--(no se confunda, findMin devuelve al tripulante con mayor rango).
--a) Escribir los invariantes de representación para poder crear elementos válidos del TAD.
--b) Implementar las funciones de la interfaz, calculando eficiencia. Justifique en cada caso la eficiencia obtenida.

data Nave = MkN (Map Sector (Set Tripulante)) (Heap Tripulante) (Sector, Int)

{-
INV REP: MkN mst ht sm
  * no pueden existir tripulantes en los sectores de mst que no esten en la heap ht
  * no existen dos o mas claves sector en mst que en su valor contengan a un mismo tripulante
  * el sector en sm debe estar en mst como clave.
-}

naveVacia :: [Sector] -> Nave
--Propósito: Crea una nave con todos esos sectores sin tripulantes.
--Precondición: la lista de sectores no está vacía
--Costo: O(S log S) siendo S la cantidad de sectores de la lista.
-- emptyH O(1) 
-- emptyM O(1)
-- emptyS O(1)
--agregarSectores O(S log S), se trata de las mismas S ya que la nave al final cuenta con todos los 
--     sectores de la lista dada
-- assocM O(1), ya que en esta llamada seria su mejor caso, cuando el map no tiene ningun elemento.
--  O(1 + 1 +1+ 1 + S log S) => se desprecian los costos lineales => O(S log S)
naveVacia (s:ss) = agregarSectores ss (MkN (assocM s emptyS emptyM) emptyH (s,0))


agregarSectores :: [Sector] -> Nave -> Nave
--PROP: agrega los sectores de la lista dada a la nave dada
-- PREC : la nave contiene al menos un sector
-- COSTO : 
-- recursion sobre lista de sectores O(S) siendo S la longitud de la lista
-- agregarSector O(log S') siendo S' cantidad de sectores en la nave
-- O(S log S') ya que realiza una operacion logaritmica por cada elemento de la lista

agregarSectores [] n = n
agregarSectores (s:ss) n = agregarSectores (agregarSector s n)

agregarSector :: Sector -> Nave -> Nave 
-- PROP: agrega el sector dado a la nave dada
-- OBS : el sector al ser nuevo no tiene tripulantes
-- PREC : la nave contiene al menos un sector
-- COSTO: 
-- fst O(1)
-- assocM O(log S) siendo S la cantidad de sectores en la nave
-- emptyS O(1)
-- O(log S + 1 + 1) = O(log S)
agregarSector s (MkN mst ht sm) = if s == fst sm
                                  then (MkN mst ht sm)
                                  else (MkN (assocM s emptyS mst) ht sm)


---------------------------------------------------------------------------------------------------

tripulantesDe :: Sector -> Nave -> Set Tripulante
--Propósito: Obtiene los tripulantes de un sector.
--Costo: O(log S) siendo S la cantidad de sectores.
-- hereda el costo de la unica subtarea que utiliza, lookupM O(log S)
tripulantesDe s (MkN mst _ _) = lookupM mst 

---------------------------------------------------------------------------------------------------

sectores :: Nave -> [Sector]
--Propósito: Denota los sectores de la nave
--Costo: O(S) siendo S la cantidad de sectores.
-- hereda el costo de la unica subtarea que utiliza, domM O(S)
sectores (MkN mst _ _) = domM mst

---------------------------------------------------------------------------------------------------

conMayorRango :: Nave -> Tripulante
--Propósito: Denota el tripulante con mayor rango.
--Precondición: la nave no está vacía.
--Costo: O(1).
-- hereda el costo de la unica subtarea que utiliza, findMin O(1)
conMayorRango (MkN _ ht _) = findMin ht

---------------------------------------------------------------------------------------------------

conMasTripulantes :: Nave -> Sector
--Propósito: Denota el sector de la nave con más tripulantes.
--Costo: O(1).
-- hereda el costo de la unica subtarea que utiliza, fst O(1)
conMasTripulantes (MkN _ _ sm) = fst sm

---------------------------------------------------------------------------------------------------

conRango :: Rango -> Nave -> Set Tripulante
--Propósito: Denota el conjunto de tripulantes con dicho rango.
--Costo: O(P log P) siendo P la cantidad de tripulantes.
-- -- hereda el costo de la unica subtarea que utiliza, losDeRango O(P log P) 
conRango r (MkN _ ht _) = losDeRango r ht 

losDeRango :: Rango -> Heap Tripulante -> Set Tripulante
-- PROP: Denota el conjunto de tripulantes con dicho rango que esten en la heap dada
-- COSTO: 
-- recursion sobre la heap O(P) siendo p la cantidad e tripulantes en la heap
-- deleteMin O(log P)
-- findMin O(1)
-- O(P * (log P +1)) => se desprecia el 1 => O(P log P)
losDeRango r ht = if isEmptyH ht
                  then emptyS 
                  else let min = findMin ht in
                       if r == (rango min)
                       then addS min (losDeRango r (deleteMin) ht)


---------------------------------------------------------------------------------------------------

sectorDe :: Tripulante -> Nave -> Sector
--Propósito: Devuelve el sector en el que se encuentra un tripulante.
--Precondición: el tripulante pertenece a la nave.
--Costo: O(S log S log P) siendo S la cantidad de sectores y P la cantidad de tripulantes.
-- domM O(S) siendo S la cantidad de sectores en la nave
-- enCualEsta O(S * log S' * log P), pero como S' en esta llamada depende de domM,
-- y por tanto coinciden S y S' => 
--  => O(S * log S * log P) => O(S log S log P)
sectorDe t (MkN mst _ _) = enCualEsta (domM mst) t mst


enCualEsta :: [Sector] -> Tripulante -> Map Sector (Set Tripulante) -> Sector
-- PROP : denota en cual sector de los dados se encuentra el tripulante en la nave
-- PREC: el tripulante esta en alguno de los sectores dados
-- COSTO : 
-- recursion sobre lista de sectores O(S) siendo S su longitud
-- lookupM O(log S') siendo S' la cantidad de sectores en la nave 
-- belongs O(log P), siendo P la cantidad de tripulantes en el map
-- =>
-- O(S * log S' * log P), realiza dos operaciones logaritmicas sobre cada elemento de la lista
enCualEsta [] t m = error "el tripulante no esta en ninguno de los sectores dados"
enCualEsta (s:ss) t m = if belongs t (lookupM s m)
                           then s 
                           else enCualEsta ss t m  

---------------------------------------------------------------------------------------------------

agregarTripulante :: Tripulante -> Sector -> Nave -> Nave
--Propósito: Agrega un tripulante a ese sector de la nave.
--Precondición: El sector está en la nave y el tripulante no.
--Costo: No hay datos (justifique su elección)
agregarTripulante t s (MkN mst ht sm) = MkN (agregarEnSector t s mst) (insertH t ht) (actualizar sm )


agregarEnSector :: Tripulante -> Sector -> Map Sector (Set Tripulante) -> Map Sector (Set Tripulante)
-- PROP: agregar el tripulante al sector dado si es que este no se encuentra en ningun otro sector
-- PREC: el tripulante no se encuentra en ningun sector
-- COSTO: siendo S los sectores en el map y P la cantidad de tripulantes por sector
-- lookupM O(log S)
-- addSet O(log P)
-- assocM O(log S)
-- domM (S), siendo S la cantidad de sectores en el map
-- estaEn O(S' * (log P + log S)), pero como S' depende de domM =>
-- O(S * (log P + log S))
-- total :
-- O( logS + log S + S + log P + S * (log P + log S))  | se desprecia log S contra S
-- O(S + log P + S * (log P + log S))                  | se desprecia S contra S * x
-- total : O(log P + S * (log P + log S))
agregarEnSector t s ms = if not (estaEn t (domM) ms)
                         then asoccM s (addSet s (lookupM s ms)) ms
                         else error "el tripulante ya se encuentra en algun sector"


estaEn :: Tripulante ->[Sector] ->  Map Sector (Set Tripulante) -> Bool
-- PROP: indica si el tripulante se encuentra en alguno de lo sectores dados dentro del map
-- COSTO: 
-- recursion sobre lista de sectores O(S') siendo S' su longitud
-- belongs O(log P) siendo P la cantidad de tripulantes por sector
-- lookupM O(log S) siendo S la cantidad de sectores en el map
-- O(S' * (log P + log S))

estaEn _ [] _ = False
estaEn t (s:ss) ms = if belongs t (lookupM s ms) 
                     then True 
                     else estaEn t ss ms