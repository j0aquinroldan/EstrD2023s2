module Organizador

{-
c) Implementar una variante del TAD Organizador suponiendo que en la interfaz del TAD Organizador se agrega una nueva
operación:

Esto puede requerir modificar el tipo de representación, agregar invariantes, y modificar operaciones existentes. Reescribir
sólo las operaciones que tienen cambios sustanciales y no en las que, por ejemplo, sólo se modifica un pattern matching.
-}

(Organizador,nuevo, agregarPrograma, todosLosProgramas, autoresDe, programasDe, programaronJuntas,  nroProgramasDePersona)
where
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

{-
INV REP: MkO cxp pxc
  * la clave persona de pxc debe pertenecer a alguno de los set valores del map cxp
  * la clave checksum de cxp debe pertenecer a alguno de los set valores del map pxc
-}


elMayorPrograma :: Organizador -> Maybe Checksum
--Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
--Nothing si no puede devolver un programa.
--Eficiencia: O(1) en peor caso.

nuevo :: Organizador
--Propósito: Un organizador vacío.
--Eficiencia: O(1)
nuevo = MkO emptyM emptySet
---------------------------------------------------------------------------------------------------------------------------------------------------------


agregarPrograma :: Organizador -> Checksum -> Set Persona -> Organizador
--Propósito: Agrega al organizador un programa con el Checksum indicado; el conjunto es el conjunto de personas autores
--de dicho programa.
--Precondición: el identificador del programa que se agrega no fue usado previamente en el organizador, y el Set de personas
--no está vacío.
--Eficiencia: no hay ninguna garantía de eficiencia.
-- O (
--    log C + # por lookupM y assocM sobre claces de cxp
-- p + # por setToList sobre la cardinalidad del set de personas
--     ( p* (log P + log c)) # por uso de registrar registrarAutoria, siendo p la cardinalidad del set de personas
-- => 
--    log C + p + ( p* (log P + log c)) => se desprecia p por estar en otro termino
--    log C + ( p* (log P + log c))
--) 
agregarPrograma (MkO cxp pxc) ck ps = 
    case lookupM c cxp of
        Just _ -> error "el checksum esta registrado"
        Nothing -> MkO 
                   (assocM ck ps cxp) 
                   (registrarAutoria (set2list ps) ck pxc)

-- o(
-- p* (# la cantidad de personas de la lista provista, por ...)
-- log P + # por looukupM y assocvM sobre las claves de pxc
-- log c # por addS sobre maximo set de checksum 
-- )) =>
--  O ( p* (log P + log c))
--)
registrarAutoria :: [Persona] -> Checksum -> Map Persona (Set Cheksum) -> Map Persona (Set Cheksum)
registrarAutoria [] _ _ pxc = pxc
registrarAutoria (p:ps) ck pxc = case lookupM p pxc of
                                    Just sc -> assocM p (addS ck sc)       (registrarAutoria ps ck pxc)
                                    Nothing -> assocM p (addS ck emptySet) (registrarAutoria ps ck pxc)

-------------------------------------------------------------------------------------------------------------------------------------------------

todosLosProgramas :: Organizador -> [Checksum]
--Propósito: denota una lista con todos y cada uno de los códigos identificadores de programas del organizador.
--Eficiencia: O(C) en peor caso, donde C es la cantidad de códigos en el organizador.
todosLosProgramas (MkO cxp _) = keys cxp 

---------------------------------------------------------------------------------------------------------------------------------

autoresDe :: Organizador -> Checksum -> Set Persona
--Propósito: denota el conjunto de autores que aparecen en un programa determinado.
--Precondición: el Checksum debe corresponder a un programa del organizador.
--Eficiencia: O(log C) en peor caso, donde C es la cantidad total de programas del organizador.
autoresDe (MkO cxp _) c = lookupM c cxp

---------------------------------------------------------------------------------------------------------------------------------

programasDe :: Organizador -> Persona -> Set Checksum
--Propósito: denota el conjunto de programas en los que participó una determinada persona.
--Precondición: la persona debe existir en el organizador.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad total de personas del organizador.
programasDe (MkO _ pxc) p = lookupM p pxc 

---------------------------------------------------------------------------------------------------------------------------------

programaronJuntas :: Organizador -> Persona -> Persona -> Bool
--Propósito: dado un organizador y dos personas, denota verdadero si ambas son autores de algún software en común.
--Precondición: las personas deben ser distintas.
--Eficiencia: O(log P + C log C) en peor caso, donde P es la cantidad de personas distintas que aparecen en todos los
--programas del organizador, y C la cantidad total de programas.
programaronJuntas (MkO cxp pxc) p1 p2 = if p1 == p2
                                        then error "son la misma persona"
                                        else 
                                            let ps1 = lookupSet p1 pxc 
                                                ps2 = lookupSet p1 pxc 
                                                in 
                                                    not (isEmptyS (intersection ps1 ps2 ))

-- O(log k) siendo k la cantidad de claves del map )
lookupSet  :: (Eq k, Eq a) => k -> Map k ( Set a ) -> Set addS
lookupSet x m = case lookupM x m of 
                Just s -> s
                Nothing -> emptyS

---------------------------------------------------------------------------------------------------------------

nroProgramasDePersona :: Organizador -> Persona -> Int
--Propósito: dado un organizador y una persona, denota la cantidad de programas distintos en los que aparece.
--Eficiencia: O(log P) en peor caso, donde P es la cantidad de personas del organizador.
nroProgramasDePersona (MkO _ pxc) p = sizeS $ lookupM p pxc


{-







-}