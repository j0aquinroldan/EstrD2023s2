
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))

-- 
--
--
--
--
--
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


------------------------------------------------------------------------------------------------------------------------

-- USEEER
-- O(
--   log P + log P # por programasDe 
--   c log c ) # por intersection sobre sets de cheksums
-- O(log P + c log c )
 
programasEnComun :: Persona -> Persona -> Organizador -> Set Checksum
-- Propósito: dadas dos personas y un organizador, denota el conjunto de aquellos programas en las que las personas
--programaron juntas.
programaronJuntas p1 p2 o = intersection (programasDe p1 o) (programasDe p2 o)






{-
Ejercicios
Recordatorio: De existir, agregue las precondiciones en las funciones solicitadas. ¡No deje de dividir en subtareas! Y no olvide
además incluir propósito y precondiciones de las funciones auxiliares que necesite programar.
a) Implementar las siguientes funciones como usuario del TAD Organizador, establecer su eficiencia y justificarla:
a) 
b) esUnGranHacker :: Organizador -> Persona -> Bool
Propósito: denota verdadero si la persona indicada aparece como autor de todos los programas del organizador.
b) Implementar el TAD Organizador suponiendo el siguiente tipo de representación:
data Organizador = MkO (Map Checksum (Set Persona)) (Map Persona (Set Checksum))
a) Escribir los invariantes de representación para poder crear elementos válidos del TAD.
b) Implementar las funciones de la interfaz, respetando las restricciones de eficiencia pedidas. Justifique en cada caso por
qué se obtiene la eficiencia buscada.
c) Implementar una variante del TAD Organizador suponiendo que en la interfaz del TAD Organizador se agrega una nueva
operación:
elMayorPrograma :: Organizador -> Maybe Checksum
Propósito: recibe un organizador y denota uno de los programas con más autores de todo ese organizador; denota
Nothing si no puede devolver un programa.
Eficiencia: O(1) en peor caso.
Esto puede requerir modificar el tipo de representación, agregar invariantes, y modificar operaciones existentes. Reescribir
sólo las operaciones que tienen cambios sustanciales y no en las que, por ejemplo, sólo se modifica un pattern matching.
Recordatorio: las interfaces de los TADs Set y Map.
La interfaz de Set, siendo N la cantidad de elementos del conjunto:
emptyS :: Set a O(1)
isEmptyS :: Set a -> Bool O(1)
addS :: a -> Set a -> Set a O(log N)
belongs :: a -> Set a -> Bool O(log N)
union :: Set a -> Set a -> Set a O(N log N)
intersection :: Set a -> Set a -> Set a O(N log N)
set2list :: Set a -> [a] O(N)
sizeS :: Set a -> Int O(1)
La interfaz de Map, siendo M la cantidad de claves distintas en el map:
emptyM :: Map k v O(1)
assocM :: k -> v -> Map k v -> Map k v O(log M)
lookupM :: Map k v -> k -> Maybe v O(log M)
domM :: Map k v -> [k] O(M)
-}



---------------------------
-- ejemplo user tree

data Tree a = EmptyT | NodeT a (Tree a) (Tree a) 

caminoalosquecontienen :: Eq a => a-> Tree (Set a) ->  [[Dir]]
caminoalosquecontienen EmptyT = []
caminoalosquecontienen x (NodeT s ti td) = let hs = (agregarACada Izq (caminoalosquecontienen x ti)) ++
                                                (agregarACada Der (caminoalosquecontienen x td))
                                           in if belongs x s 
                                           then [] : hs
                                           else hs

agregarACada :: a -> [[a]] -> [[a]]
agregarACada x [] = []
agregarACada x (ys : yss) = (x : ys) : (agregarACada x yss)