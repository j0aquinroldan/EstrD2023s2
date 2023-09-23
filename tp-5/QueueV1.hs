module QueueV1
(Queue, emptyQ, isEmptyQ, enqueue, firstQ,dequeue)
where

data Queue a = Q [a] 

{-
3. Queue (cola)
Una Queue es un tipo abstracto de datos de naturaleza FIFO (rst in, rst out). Esto signica
que los elementos salen en el orden con el que entraron, es decir, el que se agrega primero es el
primero en salir (como la cola de un banco). Su interfaz es la siguiente:


1. Implemente el tipo abstracto Queue utilizando listas. Los elementos deben encolarse por el
nal de la lista y desencolarse por delante.
-}

emptyQ :: Queue a
--Crea una cola vacía.
isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.
enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.
firstQ :: Queue a -> a
--Dada una cola devuelve el primer elemento de la cola.
dequeue :: Queue a -> Queue a
--Dada una cola la devuelve sin su primer elemento.

emptyQ = Q []

isEmptyQ (Q [])= True 
isEmptyQ _ = False

enqueue x (Q xs) = Q (xs ++ [x])

firstQ (Q (x:xs)) = x
firstQ (Q _) = error "cola vacia" 

dequeue (Q (x:xs)) = (Q xs)