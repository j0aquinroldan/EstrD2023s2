import SetV2
import QueueV1
import StackV1

--1. Cálculo de costos
{-
head' :: [a] -> a
head' (x:xs) = x -- constante

sumar :: Int -> Int
sumar x = x + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 -- constante

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1) -- lineal

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs -- lineal

factoriales :: [Int] -> [Int]
factoriales [] = []
factoriales (x:xs) = factorial x : factoriales xs -- cuadratico

pertenece :: Eq a => a -> [a] -> Bool
pertenece n [] = False
pertenece n (x:xs) = n == x || pertenece n xs -- lineal

sinRepetidos :: Eq a => [a] -> [a] -- cuadratico
sinRepetidos [] = []
sinRepetidos (x:xs) = if pertenece x xs
                      then sinRepetidos xs
                      else x : sinRepetidos xs


-- equivalente a (++)
append :: [a] -> [a] -> [a]    -- lineal
append [] ys = ys
append (x:xs) ys = x : append xs ys

concatenar :: [String] -> String  -- lineal
concatenar [] = []
concatenar (x:xs) = x ++ concatenar xs

takeN :: Int -> [a] -> [a]  -- lineal
takeN 0 xs = []
takeN n [] = []
takeN n (x:xs) = x : takeN (n-1) xs

dropN :: Int -> [a] -> [a]   -- lineal
dropN 0 xs = xs
dropN n [] = []
dropN n (x:xs) = dropN (n-1) xs

partir :: Int -> [a] -> ([a], [a]) -- lineal
partir n xs = (takeN n xs, dropN n xs)

minimo :: Ord a => [a] -> a -- lineal
minimo [x] = x
minimo (x:xs) = min x (minimo xs)

sacar :: Eq a => a -> [a] -> [a]  --  lineal
sacar n [] = []
sacar n (x:xs) =
                   if n == x
                   then xs
                   else x : sacar n xs

ordenar :: Ord a => [a] -> [a]  -- cuadratica
ordenar [] = []
orderar xs =
            let m = minimo xs
            in m : ordenar (sacar m xs)




3. Implementar la variante del tipo abstracto Set que posee una lista y admite repetidos. En
otras palabras, al agregar no va a chequear que si el elemento ya se encuentra en la lista, pero
sí debe comportarse como Set ante el usuario (quitando los elementos repetidos al pedirlos,
por ejemplo). Contrastar la eciencia obtenida en esta implementación con la anterior.
-}

--2. Como usuario del tipo abstracto Set implementar las siguientes funciones:




ejemplo :: Set Int
ejemplo2 :: Set Int
ejemplo3 :: Set Int
ejemplo = addSet 1 emptySet 
ejemplo2 = agregarTodos [2,3] emptySet
ejemplo3 = addSet 4 emptySet


losQuePertenecen :: Eq a => [a] -> Set a-> [a]
--Dados una lista y un conjunto, devuelve una lista con todos los elementos que pertenecen
--al conjunto.
losQuePertenecen [] s     = []
losQuePertenecen (x:xs) s = singularSi x (belongs x s) ++ losQuePertenecen xs s


singularSi :: a->Bool->[a]
singularSi x True = [x]
singularSi _ _ = []


sinRepetidos :: Eq a => [a] -> [a]
--Quita todos los elementos repetidos de la lista dada utilizando un conjunto como estructura auxiliar.
sinRepetidos xs = setToList (agregarTodos xs emptySet)
                   
agregarTodos :: Eq a => [a] -> Set a -> Set a
agregarTodos [] s = s 
agregarTodos (x:xs) s = agregarTodos xs (addSet x s)


data Tree a = EmptyT | NodeT a (Tree a) (Tree a) deriving Show

unirTodos :: Eq a => Tree (Set a) -> Set a
--Dado un arbol de conjuntos devuelve un conjunto con la union de todos los conjuntos
--del arbol.
unirTodos  EmptyT = emptySet
unirTodos (NodeT s izq der) = unionSet s (unionSet (unirTodos izq) (unirTodos der))

arbolSet = NodeT ejemplo EmptyT EmptyT
arb2 = NodeT ejemplo2 EmptyT arbolSet
arb3 = NodeT ejemplo3 arb2 arbolSet



--3

ejQ :: Queue Int
ejQ = enqueue 1 emptyQ

{-
3. Como usuario del tipo abstracto Queue implementar las siguientes funciones:


-}

lengthQ :: Queue a -> Int
--Cuenta la cantidad de elementos de la cola.
lengthQ q = if isEmptyQ q
            then 0
            else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
--Dada una cola devuelve la lista con los mismos elementos,
--donde el orden de la lista es el de la cola.
queueToList q = if isEmptyQ q
               then []
               else firstQ q : queueToList ( dequeue q)

unionQ :: Queue a -> Queue a -> Queue a
--Inserta todos los elementos de la segunda cola en la primera.
unionQ q1 q2 = if isEmptyQ q2
               then q1
               else unionQ (enqueue (firstQ q2) q1) (dequeue q2)

-- ==============================================================================
-- ==============================================================================

ejS :: Stack Int
ejS = push 1 emptyS

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs 

--1. Como usuario del tipo abstracto Stack implementar las siguientes funciones:


apilar :: [a] -> Stack a
--Dada una lista devuelve una pila sin alterar el orden de los elementos.
apilar xs = apilarStack xs emptyS

apilarStack :: [a] -> Stack a -> Stack a
apilarStack [] s = s
apilarStack xs s = apilarStack (init xs) (push (last xs) s)  

desapilar :: Stack a -> [a]
--Dada una pila devuelve una lista sin alterar el orden de los elementos.
desapilar s = desapilarStack s []

desapilarStack :: Stack a -> [a] -> [a]
desapilarStack s xs = if isEmptyS s 
                      then xs
                      else (top s) : (desapilarStack (pop s) xs)


insertarEnPos :: Int -> a -> Stack a -> Stack a
--Dada una posicion válida en la stack y un elemento, ubica dicho elemento en dicha
--posición (se desapilan elementos hasta dicha posición y se inserta en ese lugar).
insertarEnPos 0 x st = push x st
insertarEnPos n x st = push (top st) (insertarEnPos (n-1) x (pop st)) 

