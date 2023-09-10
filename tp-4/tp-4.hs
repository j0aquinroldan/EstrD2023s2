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

{-
2. Mapa de tesoros (con bifurcaciones)
Un mapa de tesoros es un árbol con bifurcaciones que terminan en cofres. Cada bifurcación y
cada cofre tiene un objeto, que puede ser chatarra o un tesoro.


Denir las siguientes operaciones:





6. todosLosCaminos :: Mapa -> [[Dir]]
Devuelve todos lo caminos en el mapa.

-}

data Dir = Izq | Der deriving Show
data Objeto = Tesoro | Chatarra deriving Show
data Cofre = Cofre [Objeto] deriving Show
data Mapa = Fin Cofre | Bifurcacion Cofre Mapa Mapa deriving Show

c1 = Cofre [Chatarra, Chatarra]
c2 = Cofre [Chatarra, Chatarra,Tesoro]

m1 = Fin c1
m2 = Fin c2
m3 = Bifurcacion c1 m1 m2
m4 = Bifurcacion c1 m3 m1
m5 = Bifurcacion c2 m3 m1


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
