module QueueV3
(Queue, emptyQ, isEmptyQ, enqueue, firstQ,dequeue)
where

data Queue a = QQ [a] [a]

{-
INV REP:  QQ fs bs
si fs esta vacia bs esta vacia
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

emptyQ = QQ [] []

isEmptyQ (QQ fs _) = null fs

enqueue x (QQ fs bs) = if null fs 
                       then QQ [x] []
                       else QQ fs (x:bs)
 
firstQ (QQ fs _) = head fs

dequeue (QQ fs bs) = if null (tail fs) 
                     then QQ (reverse bs) [] 
                     else QQ (tail fs) bs