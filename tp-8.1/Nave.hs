module Nave 

where

data Nave = N (Map SectorId Sector) (Map Nombre Tripulante) (MaxHeap Tripulante)

{-
INV. REP.: N ms mt ht
  * pueden existir valores sector en ms que no pertenezcan a ninguno de los valores 
     tripulante en mt, lo que significaria un sector vacio.
  * no pueden haber sectores en algun valor tripulante en el map mt que no este como valor sector dentro del
    map ms. (no pueden haber tripulantes que trabajen en sectores ajenos a la nave)
  * la clave nombre en mt debe coincidir con el nombre del valor tripulante
  * cada valor trpulante en el map mt deben coincidir con uno de los tripulantes en la maxheap ht y viceversa
-}

--b) 
construir :: [SectorId] -> Nave
--Propósito: Construye una nave con sectores vacíos, en base a una lista de identificadores de sectores.
--Eficiencia: O(S)
-- agregarSectores en el peor caso es O(S log s),
-- pero al usar la funcion con una nave vacia se asegura el mejor caso que seria O(s * 1),
-- por tanto es O(S).
construir sids = agregarSectores sids (N emptyM emptyM emptyH)

agregarSectores :: [SectorId] -> Nave -> Nave
-- PROP: agrega la lista de sectores a la nave dada
-- COSTO: O(S log s).
-- siendo S la longitud de la lista de sectores y 
-- log s que viene de aregarSector O(s), siendo s la cantidad de sectores en ms
agregarSectores [] n = n 
agregarSectores (sid:sids) n = agregarSectores sids (agregarSector sid n)


agregarSector :: SectorId -> Nave -> Nave
-- PROP: agrega un sector a la nave
--COSTO: O(log s) que es el costo de assocM y siendo s la cantidad de sectores en ms
agregarASector sid (N ms mt ht) = N (assocM sid (crearS sid) ms) mt ht

------------------------------------------------------------------------------------------------------

--c) 
ingresarT :: Nombre -> Rango -> Nave -> Nave
--Propósito: Incorpora un tripulante a la nave, sin asignarle un sector.
--Eficiencia: O(log T)
-- crearT O(1)
-- assocM O(log T), siendo T la cantidad de nombres en mt,
-- insertH O(log T), siendo T la cantidad de tripulantes en ht
-- se usa T para el costo de ambas funciones ya que por inv rep deben tener ambas la misma cantidad de 
-- elementos (los tripulantes deben estar en ambas estructuras, mt y ht)
ingresarT n r (N ms mt ht) = let trip = crearT n r in 
                             N ms (assocM n trip mt) (insertH trip ht)

---------------------------------------------------------------------------------------------------------------

--d) 
sectoresAsignados :: Nombre -> Nave -> Set SectorId
--Propósito: Devuelve los sectores asignados a un tripulante.
--Precondición: Existe un tripulante con dicho nombre.
--Eficiencia: O(log N)
-- lookupM O(log N) N siendo la cantidad de nombres en el map mt
-- sectoresT O(1)
-- O(log N) + O(1) = O(log N)
sectoresAsignados n (N _ mt _) = sectoresT (lookupM n mt)

------------------------------------------------------------------------------------------------------

--e)
datosDeSector :: SectorId -> Nave -> (Set Nombre, [Componente])
--Propósito: Dado un sector, devuelve los tripulantes y los componentes asignados a ese sector.
--Precondición: Existe un sector con dicho id.
--Eficiencia: O(log S)
--lookupM O(log S) siendo S los sectores en ms
--tripulantesS O(1)
--componentesS O(1)
-- O(log S) + O(1) + O(1) = O(log S)

datosDeSector sid (N ms _ _) = let s = (lookupM sid ms) in
                             (tripulantesS s, componentesS s)

------------------------------------------------------------------------------------------------------

--f) 
tripulantesN :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes ordenada por rango, de mayor a menor.
--Eficiencia: O(log T)
-- maxHeap2List O(log T) siendo T los tripulantes de la heap ht
tripulantesN (N _ _ ht) = maxHeap2List ht

maxHeap2List :: MaxHeap a -> [a]
-- PROP: devuevle todos los elementos de la heap como una lista, ordenados de mayor a menor
-- COSTO: 
-- isEmptyH O(1)
-- maxH O(1)
-- deleteMaxH O(log M), siendo M la cantidad de elementos en la heap
-- => costo total = O(log M)
maxHeap2List mh = if isEmptyH mh 
                  then []
                  else maxH mh : maxHeap2List (deleteMaxH mh) 


------------------------------------------------------------------------------------------------------

--g) 
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Asigna una lista de componentes a un sector de la nave.
--PREC: el sector debe existir en la nave
--Eficiencia: O(C + log S), siendo C la cantidad de componentes dados.
-- lookupM O(log S), siendo S lla cantidad de sectores en ms
-- agregarComponentes O(C), siendo C la longitud de la lista de componentes 
-- assocM O(log S), siendo S la cantidad de sectores en ms
-- =>  O(C + log S + log S) => O(C + 2 log S) => O(C + log S)
agregarASector cs sid (N ms mt ht) = 
                                    case lookupM sid ms  of
                                    Nothing -> error "el sector no existe"
                                    Just s -> let sec = (agregarComponentes cs s) in
                                              N (assocM sid sec ms ) mt ht 

agregarComponentes :: [Componente] -> Sector -> Sector
--PROP : Asigna una lista de componentes a un sector dado
-- COSTO: O(C*1), siendo C la longitud de la lista al realizar una recursion estructural y el 1 que es
-- el costo de agregarC => O(C)
agregarComponentes [] s     = s
agregarComponentes (c:cs) s = agregarC c ( agregarComponentes cs s) 


------------------------------------------------------------------------------------------------------
--h) 
asignarASector :: Nombre -> SectorId -> Nave -> Nave
--Propósito: Asigna un sector a un tripulante.
--Nota: No importa si el tripulante ya tiene asignado dicho sector.
--Precondición: El tripulante y el sector existen.
--Eficiencia: O(log S + log T + T log T)
-- lookupM O(log T) O(log S), siendo T la cantidad de tripulantes de la nave y S la cantidad de sectores
-- assocM O(log T) O(log S), siendo T la cantidad de tripulantes de la nave y S la cantidad de sectores
-- agregarT O(log T'), siendo T' la cantidad de tripulantes en el sector
-- asignarS O(log S2), siendo S2 la cantidad de sectores del tripulante
-- insertH O(log T), siendo T la cantidad de tripulantes de la nave
asignarASector n sid (N ms mt ht) = let t = lookupM n mt 
                                        s = lookupM sid ms
                                    in 
                                        N (assocM sid (agregarT n s) ms) 
                                          (assocM n (asignarS sid t) mt)
                                          (insertH  (asignarS sid t) ht)  

-- ?????????????????????


{-

-}