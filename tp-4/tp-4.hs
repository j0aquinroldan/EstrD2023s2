{-

Tenemos los siguientes tipos de datos:

Denir las siguientes funciones:






-}

-- 1. Pizzas

data Pizza = Prepizza | Capa Ingrediente Pizza deriving Show

data Ingrediente = Salsa | Queso | Jamon | Aceitunas Int deriving Show

p1 = Prepizza
p2 = Capa Salsa p1
p3 = Capa Queso p2

cantidadDeCapas :: Pizza -> Int
--Dada una pizza devuelve la cantidad de ingredientes
cantidadDeCapas Prepizza = 0
cantidadDeCapas (Capa i p) = 1 + cantidadDeCapas p

armarPizza :: [Ingrediente] -> Pizza
--Dada una lista de ingredientes construye una pizza
armarPizza [] = Prepizza
armarPizza (i : is) = Capa i (armarPizza is)

sacarJamon :: Pizza -> Pizza
--Le saca los ingredientes que sean jamón a la pizza
sacarJamon Prepizza = Prepizza
sacarJamon (Capa i p) = if esJamon i 
                      then sacarJamon p
                      else Capa i (sacarJamon p)

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _ = False


tieneSoloSalsaYQueso :: Pizza -> Bool
--Dice si una pizza tiene solamente salsa y queso (o sea, no tiene de otros ingredientes. En
--particular, la prepizza, al no tener ningún ingrediente, debería dar verdadero.)
tieneSoloSalsaYQueso Prepizza   = True
tieneSoloSalsaYQueso (Capa i p) =  esSalsaOQueso i &&  tieneSoloSalsaYQueso p


esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _ = False


duplicarAceitunas :: Pizza -> Pizza
--Recorre cada ingrediente y si es aceitunas duplica su cantidad
duplicarAceitunas Prepizza = Prepizza
duplicarAceitunas (Capa i p) = Capa (duplicarAceitunasIngrediente i) (duplicarAceitunas p)

duplicarAceitunasIngrediente :: Ingrediente -> Ingrediente
duplicarAceitunasIngrediente (Aceitunas cant) = Aceitunas (cant*2)
duplicarAceitunasIngrediente i = i


cantCapasPorPizza :: [Pizza] -> [(Int, Pizza)]
--Dada una lista de pizzas devuelve un par donde la primera componente es la cantidad de
--ingredientes de la pizza, y la respectiva pizza como segunda componente.
cantCapasPorPizza [] = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p, p) : cantCapasPorPizza ps



data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

c0 = Cofre [Chatarra, Chatarra]
c1 = Cofre [Chatarra, Chatarra,Tesoro]
c2 = Cofre [Chatarra, Tesoro,Tesoro]

m1 = Fin c0
m2 = Fin c2
m3 = Bifurcacion c0 m1 m2
m4 = Bifurcacion c0 m3 m1
m5 = Bifurcacion c0 m3 m1
m6 = Bifurcacion c2 m3 m1

--1. 
hayTesoro :: Mapa -> Bool
--Indica si hay un tesoro en alguna parte del mapa.
hayTesoro (Fin c) = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre obs) = existeTesoro obs

existeTesoro :: [Objeto]-> Bool
existeTesoro [] = False
existeTesoro (obj:objs)= esTesoro obj ||  existeTesoro objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False



--2. 
hayTesoroEn :: [Dir] -> Mapa -> Bool
--Indica si al final del camino hay un tesoro. Nota: el final de un camino se representa con una
--lista vacía de direcciones.
hayTesoroEn [] (Fin c)                   = hayTesoroEnCofre c
hayTesoroEn [] (Bifurcacion c _ _)       = hayTesoroEnCofre c
hayTesoroEn (d:ds) (Bifurcacion c m1 m2) = if esIzq d 
                                         then  hayTesoroEn ds m1
                                         else hayTesoroEn ds m2
hayTesoroEn _ _                          = False


esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _ = False 

--3. 
caminoAlTesoro :: Mapa -> [Dir]
--Indica el camino al tesoro. Precondición: existe un tesoro y es único.
caminoAlTesoro (Fin _ ) = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnCofre c
                                       then  []
                                       else  if hayTesoro m1
                                              then Izq : caminoAlTesoro m1
                                              else Der : caminoAlTesoro m2


--4.
caminoDeLaRamaMasLarga :: Mapa -> [Dir]
--Indica el camino de la rama más larga.
caminoDeLaRamaMasLarga (Fin _) = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = if heightT m1 > heightT m2 
                                               then Izq : caminoDeLaRamaMasLarga m1 
                                               else Der : caminoDeLaRamaMasLarga m2


heightT :: Mapa -> Int
--Dado un árbol devuelve su altura.
heightT (Fin _) = 0
heightT (Bifurcacion _ m1 m2) = 1 + heightT m1 + heightT m2 

--5. 
tesorosPorNivel :: Mapa -> [[Objeto]]
--Devuelve los tesoros separados por nivel en el árbol.
tesorosPorNivel (Fin c) = [tesorosEnElCofre c]
tesorosPorNivel (Bifurcacion c m1 m2)= tesorosEnElCofre c : tesorosPorNivel m1 ++ tesorosPorNivel m2

tesorosEnElCofre :: Cofre -> [Objeto]
tesorosEnElCofre (Cofre objs) = tesoros objs

tesoros :: [Objeto]->[Objeto]
tesoros [] = []
tesoros (obj:objs) = singularSi obj (esTesoro obj) ++ tesoros objs

singularSi :: a->Bool->[a]
singularSi x True = [x]
singularSi _ _ = []


--6.
todosLosCaminos :: Mapa -> [[Dir]]
--Devuelve todos lo caminos en el mapa.
todosLosCaminos (Fin _) = []
todosLosCaminos (Bifurcacion _ t1 t2) =  (agregarACada Izq (todosLosCaminos t1))  ++ 
                                         (agregarACada Der (todosLosCaminos t2))

agregarACada :: a -> [[a]] -> [[a]]
agregarACada x [] = [[x]]
agregarACada x (ys : yss) = (x : ys) : (agregarACada x yss)




--3. Nave Espacial

data Barril = Comida | Oxigeno | Torpedo | Combustible deriving Show
data Componente = LanzaTorpedos | Motor Int | Almacen [Barril] deriving Show
type Tripulante = String
type SectorId = String 
data Sector = S SectorId [Componente] [Tripulante] deriving Show
data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show
data Nave = N (Tree Sector) deriving Show


comp0 = Almacen [Comida, Oxigeno ]
comp1 = Almacen [Torpedo, Combustible ]

s0 = S "s0" [Motor 2, LanzaTorpedos, comp0] ["pepe"]
s1 = S "s1" [Motor 1, LanzaTorpedos,Motor 4, comp1] ["pipi"]

treeSector0 = NodeT s0  EmptyT   EmptyT

n0 = N (EmptyT)
n1 = N treeSector0
n2 = N (NodeT s1 treeSector0 EmptyT)

--1. 
sectores :: Nave -> [SectorId]
--Propósito: Devuelve todos los sectores de la nave.
sectores (N (EmptyT)) = []
sectores (N (NodeT s t1 t2)) = sectorId s : sectores (N t1) ++ sectores (N t2)  

sectorId (S id _ _ ) = id


--2. 
poderDePropulsion :: Nave -> Int
--Propósito: Devuelve la suma de poder de propulsión de todos los motores de la nave. Nota:
--el poder de propulsión es el número que acompaña al constructor de motores.
poderDePropulsion (N(EmptyT)) = 0
poderDePropulsion (N (NodeT s t1 t2)) = poderSector s + poderDePropulsion (N t1) + poderDePropulsion (N t2)

poderSector :: Sector -> Int
poderSector (S _ cs _) =  poderComponentes cs

poderComponentes :: [Componente] -> Int
poderComponentes [] = 0
poderComponentes (c:cs) = poderMotor c + poderComponentes cs

poderMotor (Motor p) = p
poderMotor _ = 0


--3. 
barrilesNave :: Nave -> [Barril]
--Propósito: Devuelve todos los barriles de la nave.
barrilesNave (N(EmptyT)) = []
barrilesNave (N (NodeT s t1 t2)) = barrilesSector s ++ barrilesNave (N t1) ++ barrilesNave (N t2)

barrilesSector :: Sector -> [Barril]
barrilesSector (S _ cs _) =  barrilesComponentes cs

barrilesComponentes :: [Componente] -> [Barril]
barrilesComponentes [] = []
barrilesComponentes (c:cs) = barriles c ++ barrilesComponentes cs

barriles (Almacen bs) = bs
barriles _ = []

--4.  
agregarASector :: [Componente] -> SectorId -> Nave -> Nave
--Propósito: Añade una lista de componentes a un sector de la nave.
--Nota: ese sector puede no existir, en cuyo caso no añade componentes.
agregarASector cs sId (N ts) = (N (agregarComponentesTree cs sId ts)) 


agregarComponentesTree :: [Componente] -> SectorId ->  Tree Sector -> Tree Sector
agregarComponentesTree _   _  (EmptyT)        = (EmptyT)
agregarComponentesTree cs sId (NodeT s t1 t2) = (NodeT (agregarComponentesSector cs sId s) 
                                                       (agregarComponentesTree cs sId t1 )
                                                       (agregarComponentesTree cs sId t2))             

agregarComponentesSector :: [Componente] -> SectorId ->  Sector -> Sector
agregarComponentesSector cs1 sid (S id cs2 ts) = if sid == id
                                                 then (S id (cs2 ++cs1) ts)
                                                 else (S id cs2 ts)
                                        

agregarComponentesTree2 :: [Componente] -> SectorId ->  Tree Sector -> Tree Sector
agregarComponentesTree2 _   _  (EmptyT)        = (EmptyT)
agregarComponentesTree2 cs sId (NodeT s t1 t2) = if sId == sectorId s
                                                 then (NodeT (agregarComp cs s) t1 t2)
                                                 else (NodeT s 
                                                       (agregarComponentesTree2 cs sId t1 )
                                                       (agregarComponentesTree2 cs sId t2))  
       
agregarComp cs1 (S id cs2 ts) = (S id (cs2 ++ cs1) ts)
                                                       




--5. 
asignarTripulanteANave :: Tripulante -> [SectorId] -> Nave -> Nave
--Propósito: Incorpora un tripulante a una lista de sectores de la nave.
--Precondición: Todos los id de la lista existen en la nave.--
asignarTripulanteANave t sIds (N ts) = N (asignarTripulanteATree t sIds ts)

asignarTripulanteATree :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
asignarTripulanteATree _ _ EmptyT =  EmptyT
asignarTripulanteATree tr sIds (NodeT s1 t1 t2) =NodeT (asignarTripulanteSiCorresponde tr sIds s1)
                                                       (asignarTripulanteATree tr sIds t1) 
                                                       (asignarTripulanteATree tr sIds t2)

asignarTripulanteSiCorresponde :: Tripulante -> [SectorId] ->  Sector ->  Sector
asignarTripulanteSiCorresponde _ [] s = s
asignarTripulanteSiCorresponde t (sId:sIds) s = if mismoId sId s 
                                                then asignarTripulanteASector t s
                                                else asignarTripulanteSiCorresponde t sIds s

asignarTripulanteASector t (S id cs ts) = (S id cs (t:ts))

mismoId sid s = sid == sectorId s



--6. 
sectoresAsignadosNave :: Tripulante -> Nave -> [SectorId]
--Propósito: Devuelve los sectores en donde aparece un tripulante dado dentro de la nave.
sectoresAsignadosNave tr (N ts) = sectoresAsignadosTree tr ts

sectoresAsignadosTree :: Tripulante -> Tree Sector -> [SectorId]
--Propósito: Devuelve los sectores en donde aparece un tripulante dado en el arbol de sectores.
sectoresAsignadosTree _ EmptyT =  []
sectoresAsignadosTree tr (NodeT s t1 t2)= singularSi (sectorId s) (estaAsignadoEn tr s) ++ 
                                          sectoresAsignadosTree tr t1 ++ 
                                          sectoresAsignadosTree tr t2
   
estaAsignadoEn :: Tripulante -> Sector -> Bool
estaAsignadoEn tr (S _ _ trs) = pertenece tr trs

pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = (e == x) || pertenece e xs


--7. 
tripulantesNave :: Nave -> [Tripulante]
--Propósito: Devuelve la lista de tripulantes, sin elementos repetidos
tripulantesNave (N ts) = tripulantesTree ts

tripulantesTree :: Tree Sector -> [Tripulante]
tripulantesTree EmptyT = []
tripulantesTree (NodeT s1 t1 t2) = sinRepetidos (tripulantes s1 ++ tripulantesTree t1 ++ tripulantesTree t2)

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                    then sinRepetidos xs
                    else x : sinRepetidos xs

tripulantes ::Sector -> [Tripulante]
tripulantes (S _ _ ts) = ts



--4. Manada de lobos
type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo | Explorador Nombre [Territorio] Lobo Lobo | Cria Nombre
data Manada = M Lobo



--1. Construir un valor de tipo Manada que posea 1 cazador, 2 exploradores y que el resto sean crias. 

manada0 = M caz1
caz1 = Cazador "caz" ["conejo","rata"] expl1 expl2 cria0
expl1 = Explorador "exp" ["Bosque"] cria0 cria0
expl2 =Explorador "expl2" ["Bosque"] cria0 cria0
cria0 = Cria "cria"

manada1 = M caz2
caz2 = Cazador "caz" ["conejo","rata"] expl1 expl3 cria0
expl3 =Explorador "expl2" ["Bosque"] cria0 caz2
caz3 = Cazador "caz2" [] cria0 cria0 cria0




--2. 
buenaCaza :: Manada -> Bool
--Propósito: dada una manada, indica si la cantidad de alimento cazado es mayor a la cantidad de crias.
buenaCaza (M lobo) = cantAlimento lobo > cantCrias lobo

esCria (Cria _) = True
esCria _ = False

cantAlimento :: Lobo -> Int
cantAlimento (Cazador _ presas l1 l2 l3) = longitud presas + cantAlimento l1 + cantAlimento l2 + cantAlimento l3
cantAlimento (Explorador _ _ l1 l2) = cantAlimento l1 + cantAlimento l2
cantAlimento (Cria _) = 0

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

cantCrias :: Lobo -> Int
cantCrias (Cazador _ _ l1 l2 l3) = cantCrias l1 + cantCrias l2 + cantCrias l3
cantCrias (Explorador _ _ l1 l2) = cantCrias l1 + cantCrias l2
cantCrias (Cria _) = 1


--3. 
elAlfa :: Manada -> (Nombre, Int)
--Propósito: dada una manada, devuelve el nombre del lobo con más presas cazadas, junto
--con su cantidad de presas. Nota: se considera que los exploradores y crias tienen cero presas
--cazadas, y que podrían formar parte del resultado si es que no existen cazadores con más de
--cero presas.
elAlfa (M l) = elAlfaL l

elAlfaL :: Lobo -> (Nombre, Int)
elAlfaL (Cazador nom pres l1 l2 l3) = elegirEntre (nom, longitud pres) 
                                                  (elegirEntre (elAlfaL l1 )
                                                               (elegirEntre  (elAlfaL l2) 
                                                                             (elAlfaL l3)))
elAlfaL (Explorador nom _ l1 l2) =  (elegirEntre (elAlfaL l1 )
                                                 (elegirEntre  (elAlfaL l2) 
                                                                             (nom, 0)))                              
elAlfaL (Cria nom) = (nom, 0)


elegirEntre :: (Nombre, Int) -> (Nombre, Int) -> (Nombre, Int)
elegirEntre (n1, c1) (n2, c2) = if c1 >= c2
                                then (n1,c1)
                                else (n2, c2)



--4. 
losQueExploraron :: Territorio -> Manada -> [Nombre]
--Propósito: dado un territorio y una manada, devuelve los nombres de los exploradores que
--pasaron por dicho territorio.
losQueExploraron t (M l) = losQueExploraronL t l

losQueExploraronL :: Territorio -> Lobo -> [Nombre]
losQueExploraronL t (Cazador _ _ l1 l2 l3)    = losQueExploraronL t l1 ++ 
                                                losQueExploraronL t l2 ++ 
                                                losQueExploraronL t l3 
losQueExploraronL t (Explorador nom ts l1 l2 )= singularSi nom (pertenece t ts) ++ 
                                                losQueExploraronL t l1 ++ 
                                                losQueExploraronL t l2
losQueExploraronL t (Cria nom ) = []


--5. 
exploradoresPorTerritorio :: Manada -> [(Territorio, [Nombre])]
--Propósito: dada una manada, denota la lista de los pares cuyo primer elemento es un territorio y cuyo segundo elemento es la lista de los nombres de los exploradores que exploraron
--dicho territorio. Los territorios no deben repetirse.
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioL l

exploradoresPorTerritorioL :: Lobo -> [(Territorio, [Nombre])]
exploradoresPorTerritorioL (Cazador _ _ l1 l2 l3)   = 
                                          juntarTerritorios (exploradoresPorTerritorioL l1) 
                                                (juntarTerritorios (exploradoresPorTerritorioL l2) 
                                                         (exploradoresPorTerritorioL l3) )
exploradoresPorTerritorioL (Explorador nom ts l1 l2 )= agregarExplorador nom ts 
                                                            (juntarTerritorios (exploradoresPorTerritorioL l1) 
                                                                              (exploradoresPorTerritorioL l2) )
exploradoresPorTerritorioL (Cria _ ) = []


juntarTerritorios :: [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])] 
--PROP: junta los nombres de los territorios, sin que hayan dos tuplas con el mismo territorio 
juntarTerritorios [] tns2 = tns2
juntarTerritorios tns1 [] = tns1
juntarTerritorios ((t,ns):tns) tns2= sumarTerritorio  (t,ns)  (juntarTerritorios tns tns2)
     
sumarTerritorio :: (Territorio, [Nombre]) -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
--PROP: suma los nombres del territorio al mismo territorio que esta dentro de la lista de tuplas
-- PREC: las tuplas no tienen nombres repetidos
sumarTerritorio (t,ns) []                  =  [(t,ns)]
sumarTerritorio (t,ns) ( (t2, ns2) : tns2) = if t == t2
                                             then (t2, appendSinRep ns ns2 ) :  tns2
                                             else  (t2, ns2) : sumarTerritorio (t,ns) tns2

appendSinRep :: Eq a => [a] -> [a] -> [a]
appendSinRep xs [] = xs
appendSinRep [] ys = ys
appendSinRep (x:xs) ys = if pertenece x (sinRepetidos ys) || pertenece x xs
                        then appendSinRep xs (sinRepetidos ys) 
                        else x : appendSinRep xs (sinRepetidos ys) 


agregarExplorador :: Nombre -> [Territorio] -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
--PREC: en la lista de territorios no hay repetidos y en la lista de tuplas tampoco hay territorios repetidos
agregarExplorador _ [] tns  = tns
agregarExplorador nom (t:ts) tns =  agregarATerreno nom t (agregarExplorador nom ts tns)

agregarATerreno :: Nombre -> Territorio -> [(Territorio, [Nombre])] -> [(Territorio, [Nombre])]
--PREC: en la lista de tuplas no hay territorios repetidos
agregarATerreno nom t [] = [(t, [nom])]
agregarATerreno nom t ((t2, ns):tns) = if t==t2
                                       then (t, nom :ns) : tns
                                       else (t2, ns) : agregarATerreno nom t tns



--6. 
superioresDelCazador :: Nombre -> Manada -> [Nombre]
--Propósito: dado un nombre de cazador y una manada, indica el nombre de todos los
--cazadores que tienen como subordinado al cazador dado (directa o indirectamente).
--Precondición: hay un cazador con dicho nombre y es único
superioresDelCazador nom (M l) = superioresDelCazadorLobos nom l

superioresDelCazadorLobos :: Nombre  -> Lobo -> [Nombre]
superioresDelCazadorLobos nom (Cazador nom2 _ l1 l2 l3)  = appendSinRep (
                                                            singularSi nom2 (
                                                             esOTieneSubordinadoA nom l1 ||
                                                             esOTieneSubordinadoA nom l2 ||
                                                             esOTieneSubordinadoA nom l3)) 
                                                           (superioresDelCazadorLobos nom l1 ++
                                                           superioresDelCazadorLobos nom l2 ++
                                                           superioresDelCazadorLobos nom l3)
superioresDelCazadorLobos nom (Explorador nom2 _ l1 l2 ) = appendSinRep (
                                                            singularSi nom2 (
                                                             esOTieneSubordinadoA nom l1 ||
                                                             esOTieneSubordinadoA nom l2) ) 
                                                           (superioresDelCazadorLobos nom l1 ++
                                                           superioresDelCazadorLobos nom l2 )
superioresDelCazadorLobos nom (Cria _ )                 = []

esOTieneSubordinadoA :: Nombre -> Lobo ->Bool 
esOTieneSubordinadoA nom (Cazador nom2 _ l1 l2 l3)  = nom == nom2 ||
                                                   cazadorYSeLlama nom l1 ||
                                                   cazadorYSeLlama nom l2 ||
                                                   cazadorYSeLlama nom l3                                                  
esOTieneSubordinadoA nom (Explorador nom2 _ l1 l2 ) = cazadorYSeLlama nom l1 ||
                                                   cazadorYSeLlama nom l2 
esOTieneSubordinadoA nom (Cria _ )                  = False

cazadorYSeLlama nom (Cazador nom2 _ _ _ _ )  = nom == nom2
cazadorYSeLlama nom (Explorador nom2 _ _ _ ) = False
cazadorYSeLlama nom (Cria _ )                = False

appendSinRep :: Eq a => [a] -> [a] -> [a]
appendSinRep xs [] = xs
appendSinRep [] ys = ys
appendSinRep (x:xs) ys = if pertenece x (sinRepetidos ys) || pertenece x xs
                        then appendSinRep xs (sinRepetidos ys) 
                        else x : appendSinRep xs (sinRepetidos ys) 