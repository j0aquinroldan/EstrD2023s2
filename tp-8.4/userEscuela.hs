

--j) 
hechizosAprendidos :: EscuelaDeMagia -> Set Hechizo
--Propósito: Retorna todos los hechizos aprendidos por los magos.
--Eficiencia: O(M ∗ (log M + H log H))
-- magos O(M)
-- hechizosDeLos O(N * (log M + H log H)), siendo H la cantidad de hechizos del set,
--                                         M la cantidad de magos y
--                                         N la cantidad de nombres 
-- como se le pasa por parametro magos e => N = M , por tanto ->
-- costo =    O(M ∗ (log M + H log H))  
hechizosAprendidos e = hechizosDeLos (magos e) e

hechizosDeLos :: [Nombre] -> EscuelaDeMagia -> Set Hechizo
-- PROP: devuelve los hechizos de todos los magos de la lista dada en la escuela
-- COSTO: 
-- hechizosDe O(log M), siendo M la cantidad de magos en la escuela
-- unionS O (H log H), siendo H la cantidad de hechizos en el set
-- emptyS O(1)
-- recursion sobre lista de nombres O(N), siendo N la longitud 
-- O(N * (log M + H log H))
hechizosDeLos [] _ = emptyS
hechizosDeLos (n:ns) e = unionS (hechizosDe n e) hechizosDeLos ns e

--k) 
hayUnExperto :: EscuelaDeMagia -> Bool
--Propósito: Indica si existe un mago que sabe todos los hechizos enseñados por la escuela.
--Eficiencia: O(log M)
-- nombre O(1)
-- leFaltanAprender O(log M)
-- egresarUno O(log M)
-- fst O(1)
-- O(log M + log M + 1 + 1) = > O(2*(log M)) => O(log M)
hayUnExperto e = let m = fst (egresarUno e) in
                 leFaltanAprender (nombre m) e == 0 


--l) 
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
--Propósito: Devuelve un par con la lista de magos que saben todos los hechizos dados por la escuela y la escuela sin dichos
--magos.
--Eficiencia: O(M log M)
-- recursion sobre los magos de la escuela O(M)
-- agregarAlFst O(1)
-- egresarUno O(log M)
-- hayUnExperto O(log M)
-- siendo M la cantidad de magos de la escuela
-- O( M * (1 + log M + log M)) => O( M * (2 * (log M))) => O( M * log M)
egresarExpertos e = if not (hayUnExperto e )
                    then ([], e)
                    else let (m, eSinM) = egresarUno in
                        agregarAlFst m (egresarExpertos eSinM)


{- SOLUCION FIDEL
egresarExpertos :: EscuelaDeMagia -> ([Mago], EscuelaDeMagia)
 -- Propósito: Devuelve un par con la lista de magos que saben todos
 -- los hechizos dados por la escuela y la escuela sin ellos.
 -- Eficiencia: O(M logM)
egresarExpertos escuela = if not (hayUnExperto escuela)
                          then ([], escuela)
                          else let (m , escuelaSinM) = egresarUno escuela
                                   (ms, escuelaSinMs) = egresarExpertos escuelaSinM
                               in (m:ms, escuelaSinMs)

-}                        
                         
agregarAlFst :: a -> ([a], b) -> ([a], b) 
-- costo: O(1)
agregarAlFst x (ys, z) = ((x:ys), z)


-- bonus mago

data Mago = ConsM Nombre (Set Hechizo)