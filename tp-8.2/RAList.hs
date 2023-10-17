module RAList



where


data RAList a = MkR Int (Map Int a) (Heap a)

{-
INV REP: MkR n m h
  * si n=0 entonces la RAList esta vacia
  * cada vez que se ingresa un elemento n aumenta
  * cada vez que se elimina un elemento n disminuye
  * los cambios sobre los valores de m se deben replicar en h
-}

--a) 
emptyRAL :: RAList a
--Propósito: devuelve una lista vacía.
--Eficiencia: O(1).
emptyRAL = MkR 0 emptyM emptyH

-----------------------------------------------------------------------------------------------------------

--b) 
isEmptyRAL :: RAList a -> Bool
--Propósito: indica si la lista está vacía.
--Eficiencia: O(1).
isEmptyRAL (MkR n _ _) = n == 0

-----------------------------------------------------------------------------------------------------------

--c) 
lengthRAL :: RAList a -> Int
--Propósito: devuelve la cantidad de elementos.
--Eficiencia: O(1).
lengthRAL (MkR n _ _) = n 

-----------------------------------------------------------------------------------------------------------

--d) 
get :: Int -> RAList a -> a
--Propósito: devuelve el elemento en el índice dado.
--Precondición: el índice debe existir.
--Eficiencia: O(log N).
get n (MkR n m _) = lookupM n m

-----------------------------------------------------------------------------------------------------------

--e) 
minRAL :: Ord a => RAList a -> a
--Propósito: devuelve el mínimo elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(1).
minRAL (MkR _ _ h) = findMin h 

-----------------------------------------------------------------------------------------------------------

--f) 
add :: Ord a => a -> RAList a -> RAList a
--Propósito: agrega un elemento al final de la lista.
--Eficiencia: O(log N).
add x (MkR n m h) = MkR n (assocM (n+1) x m) insertH n h

-----------------------------------------------------------------------------------------------------------

--g) 
elems :: Ord a => RAList a -> [a]
--Propósito: transforma una RAList en una lista, respetando el orden de los elementos.
--Eficiencia: O(N log N).
elems (MkR n m h) = buscar n m

buscar :: Int -> Map Int a -> [a]
buscar (-1) m = []
buscar n m = buscar (n-1) m ++ (lookupM n m)  

-----------------------------------------------------------------------------------------------------------


--h) 
remove :: Ord a => RAList a -> RAList a
--Propósito: elimina el último elemento de la lista.
--Precondición: la lista no está vacía.
--Eficiencia: O(N log N).
remove (MkR n m h) = let n' = n-1 
                     MkR n' (deleteM n' m) (borrarH (lookupM n' m) h)  


borrarH :: Ord a => Heap a -> Heap a
borrarH x h = if isEmptyH h 
              then h
              else if (findMin h) == x
                   then deleteMin h
                   else insertH (findMin h) (borrarH x (deleteMin h))

-----------------------------------------------------------------------------------------------------------

--i) 
set :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: reemplaza el elemento en la posición dada.
--Precondición: el índice debe existir.
--Eficiencia: O(N log N).
set n' x (MkR n m h) = let y = lookupM n' m in
                       MkR n (assocM n' x m) (insertH x (borrarH y h) )


--j) 
addAt :: Ord a => Int -> a -> RAList a -> RAList a
--Propósito: agrega un elemento en la posición dada.
--Precondición: el índice debe estar entre 0 y la longitud de la lista.
--Observación: cada elemento en una posición posterior a la dada pasa a estar en su posición siguiente.
--Eficiencia: O(N log N).
--Sugerencia: definir una subtarea que corra los elementos del Map en una posición a partir de una posición dada. Pasar
--también como argumento la máxima posición posible.
addAt n' x (MkR n m h) = MkR (n+1) (assocM n' x (correrDesde (n'+1) n m)) (insertH x h)   

correrDesde :: Int -> Int -> Map Int a
correrDesde n l m =  if n == l      -- n es la posicion a correr y l la maxima posicion
                       then m 
                       else assocM l (lookupM (l-1) m) (correrDesde n (l-1) m)

{-
En dicha representación se observa:
Un Int, que representa la próxima posición a ocupar en la lista. Es decir, cuando se agregue un elemento al final, debe
agregarse en dicha posición, que luego será incrementada. Cuando la estructura está vacía, el número es 0.
Un Map Int a, que representa la relación entre índices (claves) y valores de la estructura.
Una Heap a que contiene todos los valores de la estructura.
La interfaz a implementar, siendo N la cantidad de elementos, es la siguiente:




-}