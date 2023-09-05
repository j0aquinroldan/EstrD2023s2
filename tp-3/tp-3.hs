

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

data Objeto = Cacharro | Tesoro deriving Show
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino deriving Show

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
cam6 = Nada cam5

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
--Indica si hay al menos n tesoros en el camino. PRECON: n debe ser mayor a 0 
alMenosNTesoros n c = cantTesoros c >= n

cantTesoros :: Camino -> Int
cantTesoros Fin = 0
cantTesoros (Nada c) = cantTesoros c
cantTesoros (Cofre objs c ) = tesorosEnCofre objs + cantTesoros c

tesorosEnCofre :: [Objeto]->Int
tesorosEnCofre [] = 0
tesorosEnCofre (obj:objs) = unoSi(esTesoro obj) + tesorosEnCofre objs

-----------------------
cantTesorosEntre :: Int -> Int -> Camino -> Int
--Dado un rango de pasos, indica la cantidad de tesoros que hay en ese rango.
-- Precondicion: n1<n2
cantTesorosEntre n1 n2 c = if   n1<n2
                           then cantTesorosHasta n2 (caminoSinPrimeros_ n1 c)
                           else error "el primer int debe ser menor que el segundo int"

caminoSinPrimeros_ :: Int -> Camino -> Camino
-- devulve el camino sin los primeros n pasos
caminoSinPrimeros_ 0 c = c
caminoSinPrimeros_ _ Fin = Fin
caminoSinPrimeros_ n (Nada c) = caminoSinPrimeros_ (n-1) c
caminoSinPrimeros_ n (Cofre objs c) = caminoSinPrimeros_ (n-1) c


cantTesorosHasta ::  Int-> Camino -> Int
-- Devuelve cuantos tesoros hay hasta los pasos dados por parametro
cantTesorosHasta _ Fin = 0
cantTesorosHasta 0 c = unoSi(hayTesoroEn 0 c)
cantTesorosHasta n (Nada c) = cantTesorosHasta (n-1) c
cantTesorosHasta n (Cofre objs c ) = tesorosEnCofre objs + cantTesorosHasta (n-1) c

----------------------
--2. Tipos arbóreos
--2.1. Árboles binarios


data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

arb1 = EmptyT 
arb2 = NodeT (1::Int) (arb1) (arb1)
arb3 = NodeT (3::Int)  (arb1) (arb2)

--1. 
sumarT :: Tree Int -> Int
--Dado un árbol binario de enteros devuelve la suma entre sus elementos.
sumarT EmptyT = 0
sumarT (NodeT n t1 t2)= n + sumarT t1 + sumarT t2  



--2. 
sizeT :: Tree a -> Int
--Dado un árbol binario devuelve su cantidad de elementos
sizeT EmptyT = 0
sizeT (NodeT n t1 t2)= 1 + sizeT t1 + sizeT t2  

--3. 
mapDobleT :: Tree Int -> Tree Int
--Dado un árbol de enteros devuelve un árbol con el doble de cada número.
mapDobleT (EmptyT) = EmptyT
mapDobleT (NodeT n t1 t2)= (NodeT (n*2) (mapDobleT t1) (mapDobleT t2))  


--4. 
perteneceT :: Eq a => a -> Tree a -> Bool
--Dados un elemento y un árbol binario devuelve True si existe un elemento igual a ese en el
--árbol.
perteneceT _ EmptyT = False
perteneceT x1 (NodeT x2 t1 t2) = x1 == x2 || perteneceT x1 t1 || perteneceT x1 t2

--5. 
aparicionesT :: Eq a => a -> Tree a -> Int
--Dados un elemento e y un árbol binario devuelve la cantidad de elementos del árbol que son
--iguales a e.
aparicionesT _ EmptyT = 0
aparicionesT x1 (NodeT x2 t1 t2) = unoSi(x1==x2) + aparicionesT x1 t1 + aparicionesT x1 t2 

--6. 
leaves :: Tree a -> [a]
--Dado un árbol devuelve los elementos que se encuentran en sus hojas.
leaves EmptyT = []
leaves (NodeT x t1 t2) = if esVacio t1 && esVacio t2
                         then [x]
                         else leaves t1 ++ leaves t2

esVacio :: Tree a -> Bool
esVacio EmptyT = True
esVacio _ = False

--7. 
heightT :: Tree a -> Int
--Dado un árbol devuelve su altura.
heightT EmptyT = 0
heightT (NodeT x t1 t2) = 1 + heightT t1 + heightT t2 

--8. 
mirrorT :: Tree a -> Tree a
--Dado un árbol devuelve el árbol resultante de intercambiar el hijo izquierdo con el derecho,
--en cada nodo del árbol.
mirrorT EmptyT = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)


--9. 
toList :: Tree a -> [a]
--Dado un árbol devuelve una lista que representa el resultado de recorrerlo en modo in-order.
toList EmptyT = []
toList (NodeT x t1 t2) = leaves t1 ++ [x] ++ leaves t2

--10. 
levelN :: Int -> Tree a -> [a]
--Dados un número n y un árbol devuelve una lista con los nodos de nivel n. El nivel de un
--nodo es la distancia que hay de la raíz hasta él. La distancia de la raiz a sí misma es 0, y la
--distancia de la raiz a uno de sus hijos es 1.
levelN _ EmptyT = []
levelN 0 (NodeT x _ _) = [x]
levelN n (NodeT x t1 t2) =  levelN (n-1) t1 ++ levelN (n-1) t2


--11. 
listPerLevel :: Tree a -> [[a]]
--Dado un árbol devuelve una lista de listas en la que cada elemento representa un nivel de
--dicho árbol.
listPerLevel EmptyT = []
listPerLevel (NodeT n t1 t2) =  [levelN 1 (NodeT n t1 t2)] ++ listPerLevel t1 ++ listPerLevel t2
-- CONSULTAR POR LISTAS VACIAS


--12. 
ramaMasLarga :: Tree a -> [a]
--Devuelve los elementos de la rama más larga del árbol
ramaMasLarga EmptyT = []
ramaMasLarga (NodeT x t1 t2) = if   heightT t1 > heightT t2
                               then x : ramaMasLarga t1
                               else x : ramaMasLarga t2

--13. 
todosLosCaminos :: Tree a -> [[a]]
--Dado un árbol devuelve todos los caminos, es decir, los caminos desde la raíz hasta cualquiera
--de los nodos.
todosLosCaminos EmptyT = []
todosLosCaminos (NodeT x t1 t2) =  agregarACada x (todosLosCaminos t1  ++ todosLosCaminos t2)

agregarACada :: a -> [[a]] -> [[a]]
agregarACada x [] = [[x]]
agregarACada x (ys : yss) = (x : ys) : (agregarACada x yss)



--REVISAR

{-
(NodeT 1 (NodeT 2 (NodeT 3 EmptyT EmptyT)
EmptyT)
(NodeT 4 (NodeT 5 EmptyT EmptyT)
EmptyT))





-}

--2.2. Expresiones Aritméticas
data ExpA = Valor Int | Sum ExpA ExpA | Prod ExpA ExpA | Neg ExpA


--1. 
eval :: ExpA -> Int
--Dada una expresión aritmética devuelve el resultado evaluarla.
eval (Valor n) = n
eval (Neg e)   = (- eval e)
eval (Sum e1 e2) = eval e1 + eval e2
eval (Prod e1 e2) = eval e1 * eval e2


exp1 = Prod (Valor 2) (Valor 3)
exp2 = Sum (Valor 2) (Valor 3)
exp3 = Valor 3
exp4 = Neg (Valor 2) 
exp5 = Neg (Prod exp2 exp1)


--2. 
--simplificar :: ExpA -> ExpA
--Dada una expresión aritmética, la simplica según los siguientes criterios (descritos utilizando
--notación matemática convencional):

--simplificar (Neg e)      =
--simplificar (Sum 0 e2)  = e2
--simplificar (Sum e1 0)  = e1
--simplificar (Prod _ 0) = 0
--simplificar (Prod 0 _) = 0
--simplificar (Prod 1 e2)  = e2
--simplificar (Prod e1 1)  = e1
--simplificar e   = e


                             

{-
a) 0 + x = x + 0 = x
b) 0 * x = x * 0 = 0
c) 1 * x = x * 1 = x
d) - (- x) = x

-}