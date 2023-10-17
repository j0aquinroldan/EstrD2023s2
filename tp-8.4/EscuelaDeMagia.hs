module EscuelaDeMagia

where

data EscuelaDeMagia = EDM (Set Hechizo) (Map Nombre Mago) (PriorityQueue Mago)

{-
INV REP : EDM sh mm pm
  * la clave nombre en mm se corresponde con el nombre del mago en su valor
  * todos los magos en mm siempre estan en pm, por tanto tienen la misma longitud
  * no existe un mago (ni en mm ni en pm) que contenga hechizos que no esten en sh
  * no existen magos en pm con el mismo nombre
-}

--Implementación
--Implementar la siguiente interfaz de EscuelaDeMagia, utilizando la representación y los costos dados, calculando los costos
--de cada subtarea, y siendo M la cantidad de magos y H la cantidad de hechizos:


--b) 
fundarEscuela :: EscuelaDeMagia
--Propósito: Devuelve una escuela vacía.
--Eficiencia: O(1)
fundarEscuela = EDM emptyS emptyM emptyPQ


--c) 
estaVacia :: EscuelaDeMagia -> Bool
--Propósito: Indica si la escuela está vacía.
--Eficiencia: O(1)
estaVacia (EDM _ _ pm) = isEmptyPQ pm


--d) 
registrar :: Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Incorpora un mago a la escuela (si ya existe no hace nada).
--Eficiencia: O(log M)
-- insertPQ O(log M)
-- crearM O(1)
-- asoccM O(log M)
-- lookupM O(log M)
-- siendo M la cantidad de elementos en cada estructura 
-- O( log M + 1 + log M + log M ) => O(3 (log M)) => O( log M)
registrar n (EDM sh mm pm) = case lookupM n mm of 
                             Just _  -> EDM sh mm pm
                             Nothing -> let m = crearM n 
                                        in EDM sh (assocM n m mm) (insertPQ m pm)


--e) 
magos :: EscuelaDeMagia -> [Nombre]
--Propósito: Devuelve los nombres de los magos registrados en la escuela.
--Eficiencia: O(M)
magos (EDM _ mm _) = domM mm

--f) 
hechizosDe :: Nombre -> EscuelaDeMagia -> Set Hechizo
--Propósito: Devuelve los hechizos que conoce un mago dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
hechizosDe n (EDM _ mm _) = case lookupM n mm of 
                            Just n' -> hechizos n'
                            Nothing -> error "no existe tal mago"



--g) 
leFaltanAprender :: Nombre -> EscuelaDeMagia -> Int
--Propósito: Dado un mago, indica la cantidad de hechizos que la escuela ha dado y él no sabe.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(log M)
-- hechizosDe O(log M), siendo M la cantidad de magos en la escuela
-- sizeS O(1)
-- O(1 + 1 + log M) => O(log M)
leFaltanAprender n (EDM sh mm pm) = sizeS sh - sizeS (hechizosDe n (EDM sh mm pm))


--h) 
egresarUno :: EscuelaDeMagia -> (Mago, EscuelaDeMagia)
--Propósito: Devuelve el mago que más hechizos sabe y la escuela sin dicho mago.
--Precondición: Hay al menos un mago.
--Eficiencia: O(log M)
-- deleteMaxPQ O(log M), siendo M la cantidad de elementos en la pq
-- nombre O(1)
-- deleteM O(log M), siendo M la cantidad de elementos en el map
-- maxPQ O(1)
-- O(logM + log M + 1 + 1) => O(2(log M)) => O(log M)
egresarUno (EDM sh mm pm)  = let m = maxPQ pm in
                            (m , EDM sh (deleteM (nombre m) mm) (deleteMaxPQ pm) )


--i) 
enseñar :: Hechizo -> Nombre -> EscuelaDeMagia -> EscuelaDeMagia
--Propósito: Enseña un hechizo a un mago existente, y si el hechizo no existe en la escuela es incorporado a la misma.
--Nota: No importa si el mago ya conoce el hechizo dado.
--Precondición: Existe un mago con dicho nombre.
--Eficiencia: O(M log M + log H)

-- modificarPQ O(log M), siendo M la cantidad de magos en la escuela
-- aprender O(log H'), siendo H' la cantidad de hechizos del mago
-- assocM O(log M), siendo M la cantidad de magos en la escuela
-- addS O(log H), siendo H la cantidad de hechizos en la escuela
-- lookupM O(log M),siendo M la cantidad de magos en la escuela
-- O(M log M + log H)
enseñar h n (EDM sh mm pm) = case lookupM n mm of
                             Nothing -> error "no existe ese mago"
                             Just m -> let newM = aprender h m in
                                     EDM (addS h sh) (assocM n newM mm) (modificarPQ newM pm)

modificarPQ :: Mago -> PriorityQueue Mago -> PriorityQueue Mago
-- PROP: modifica la pq, ingresando el mago dado por el que tenga el mismo nombre en la pq
-- PREC : existe un mago con el mismo nombre del mago dado
-- COSTO: O(M) siendo m la cantidad de magos en la pq, ya que en peor caso debe recorrerla toda
modificarPQ m pm = let maxM = maxPQ pm in 
                   if maxM == m 
                    then insertPQ m (deleteMaxPQ pm)
                    else insertPQ maxPQ (modificarPQ m (deleteMaxPQ)) 