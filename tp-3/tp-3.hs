

--1. Tipos recursivos simples
--1.1. Celdas con bolitas
--Representaremos una celda con bolitas de colores rojas y azules, de la siguiente manera:
data Color = Azul | Rojo deriving Show
data Celda = Bolita Color Celda | CeldaVacia deriving Show

{-
En dicha representación, la cantidad de apariciones de un determinado color denota la cantidad
de bolitas de ese color en la celda. Por ejemplo, una celda con 2 bolitas azules y 2 rojas, podría
ser la siguiente:
Bolita Rojo (Bolita Azul (Bolita Rojo (Bolita Azul CeldaVacia)))
Implementar las siguientes funciones sobre celdas:
-}

celda1 = CeldaVacia
celda2 = Bolita Rojo celda1
celda3 = Bolita Rojo celda2

nroBolitas :: Color -> Celda -> Int
--Dados un color y una celda, indica la cantidad de bolitas de ese color. Nota: pensar si ya
--existe una operación sobre listas que ayude a resolver el problema.
nroBolitas _ CeldaVacia = 0
nroBolitas c1 (Bolita c2 celda) =  unoSi (mismoColor c1 c2 ) + nroBolitas c1 celda

mismoColor :: Color -> Color -> Bool 
mismoColor Azul Azul = True
mismoColor Rojo Rojo = True 
mismoColor _ _ = False

unoSi :: Bool -> Int
unoSi True = 1 
unoSi _ = 0

poner :: Color -> Celda -> Celda
--Dado un color y una celda, agrega una bolita de dicho color a la celda.
poner c celda = Bolita c celda

sacar :: Color -> Celda -> Celda
--Dado un color y una celda, quita una bolita de dicho color de la celda. Nota: a diferencia de
--Gobstones, esta función es total.
sacar _ CeldaVacia = CeldaVacia
sacar c (Bolita c2 celda) =  if mismoColor c c2 
                             then celda
                             else (Bolita c2 celda)


ponerN :: Int -> Color -> Celda -> Celda
--Dado un número n, un color c, y una celda, agrega n bolitas de color c a la celda.
ponerN 0 _ celda = celda
ponerN n c celda = Bolita c (ponerN (n-1) c celda)


--1.2. Camino hacia el tesoro

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

hayTesoro :: Camino -> Bool
--Indica si hay un cofre con un tesoro en el camino
hayTesoro Fin = False
hayTesoro (Cofre objs c) = existeTesoro objs || hayTesoro c
hayTesoro (Nada c) = hayTesoro c

existeTesoro :: [Objeto]-> Bool
existeTesoro [] = False
existeTesoro (obj:objs)= esTesoro obj ||  existeTesoro objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _ = False

cam1 = Cofre [Cacharro, Cacharro] Fin
cam2 = Nada cam1
cam3 = Fin
cam4 = Cofre [Cacharro, Tesoro] Fin
cam5 = Nada cam4

-----------------------
pasosHastaTesoro :: Camino -> Int
--Indica la cantidad de pasos que hay que recorrer hasta llegar al primer cofre con un tesoro.
--Si un cofre con un tesoro está al principio del camino, la cantidad de pasos a recorrer es 0.
--Precondición: tiene que haber al menos un tesoro.
pasosHastaTesoro Fin = error "No hay tesoro"
pasosHastaTesoro (Cofre objs c) = 0
pasosHastaTesoro (Nada c) = 1 + pasosHastaTesoro c

-------------------

hayTesoroEn :: Int -> Camino -> Bool
--Indica si hay un tesoro en una cierta cantidad exacta de pasos. Por ejemplo, si el número de
--pasos es 5, indica si hay un tesoro en 5 pasos
hayTesoroEn n c = pasosHastaTesoro c == n

-------------------

alMenosNTesoros :: Int -> Camino -> Bool
--Indica si hay al menos n tesoros en el camino.
alMenosNTesoros n c = cantTesoros c >= n

cantTesoros :: Camino -> Int
cantTesoros Fin = 0
cantTesoros (Nada c) = cantTesoros c
cantTesoros (Cofre objs c ) = tesorosEnCofre objs + cantTesoros c

tesorosEnCofre :: [Objeto]->Int
tesorosEnCofre [] = 0
tesorosEnCofre (obj:objs) = unoSi(esTesoro obj) + tesorosEnCofre objs

-----------------------
(desafío) cantTesorosEntre :: Int -> Int -> Camino -> Int
Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango. Por ejemplo, si
el rango es 3 y 5, indica la cantidad de tesoros que hay entre hacer 3 pasos y hacer 5. Están
incluidos tanto 3 como 5 en el resultado.

{-

.





-}