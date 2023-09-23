
{-
4. Stack (pila)
Una Stack es un tipo abstracto de datos de naturaleza LIFO (last in, first out). Esto significa
que los últimos elementos agregados a la estructura son los primeros en salir (como en una pila de
platos). Su interfaz es la siguiente:

-}

module StackV1
(Stack, emptyS, isEmptyS, push, top,pop, lenS)
where

data Stack a = Stack [a] Int deriving Show


emptyS :: Stack a
--Crea una pila vacía.
isEmptyS :: Stack a -> Bool
--Dada una pila indica si está vacía.
push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.
top :: Stack a -> a
--Dada un pila devuelve el elemento del tope de la pila.
pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.
lenS :: Stack a -> Int
--Dada la cantidad de elementos en la pila.
--Costo: constante.

emptyS = Stack [] 0

isEmptyS (Stack [] _) = True 
isEmptyS _ = False

push x (Stack xs n) = (Stack (x:xs) (n+1))

top (Stack (x:xs) _) = x

pop (Stack (x:xs) n) = (Stack xs (n-1))

lenS (Stack _ n) = n