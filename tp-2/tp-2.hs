--1. Recursión sobre listas

-- 1. 
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (n:ns) = n + sumatoria ns

--2.
longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

--3. 
sucesores :: [Int] -> [Int]
sucesores []= []
sucesores (x:xs)= (x+1) : (sucesores xs) 

--4. 
conjuncion :: [Bool] -> Bool
conjuncion []= True
conjuncion (x:xs) = if x
                    then conjuncion xs
                    else False

--5. 
disyuncion :: [Bool] -> Bool
disyuncion []= False
disyuncion (x:xs) = if x
                    then True
                    else disyuncion xs


--6. 
aplanar :: [[a]] -> [a]
aplanar []=[]
aplanar (x:xs) =  x ++ aplanar xs 


--7. 
pertenece :: Eq a => a -> [a] -> Bool
pertenece e [] = False
pertenece e (x:xs) = if (e == x)
                     then True
                     else pertenece e xs


--8. 
apariciones :: Eq a => a -> [a] -> Int
apariciones e [] = 0
apariciones e (x:xs) = if (e ==  x)
                     then 1 + apariciones e xs
                     else apariciones e xs

--9. 
losMenoresA :: Int -> [Int] -> [Int]
losMenoresA n []= []
losMenoresA n (m:ms)= if (n > m)
                      then m : losMenoresA n ms
                      else losMenoresA n ms


--10.
lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA n [] = []
lasDeLongitudMayorA n (x:xs) = if longitud x > n
                                then x : lasDeLongitudMayorA n xs
                                else lasDeLongitudMayorA n xs


--11.
agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal [] e = [e]
agregarAlFinal x e = x ++ [e]


--12.
agregar :: [a] -> [a] -> [a]
agregar x [] = x
agregar [] y = y 
agregar (x1:xs) y =  x1 : (agregar y xs) 


--13. 
reversa :: [a] -> [a]
reversa [] = []
reversa (x:xs) = reversa xs ++ [x]


--14. 
zipMaximos :: [Int] -> [Int] -> [Int]
--Dadas dos listas de enteros, devuelve una lista donde el elemento en la posición n es el
--máximo entre el elemento n de la primera lista y de la segunda lista, teniendo en cuenta que
--las listas no necesariamente tienen la misma longitud.
zipMaximos [] _ = []
zipMaximos _ [] = []
zipMaximos (x1 : xs) (y1:ys) = if (x1>y1)
                            then x1 : zipMaximos xs ys
                            else y1 : zipMaximos xs ys 


--15. 
elMinimo :: Ord a => [a] -> a
elMinimo [x]= x
elMinimo (x:xs) = if (x < (elMinimo xs))
                  then x
                  else elMinimo xs


-- 2. Recursión sobre números


--1. 
factorial :: Int -> Int
--Dado un número n se devuelve la multiplicación de este número y todos sus anteriores hasta
--llegar a 0. Si n es 0 devuelve 1. La función es parcial si n es negativo.
factorial 0 = 1
factorial n = n * (n-1)

--2.
cuentaRegresiva :: Int -> [Int]
--Dado un número n devuelve una lista cuyos elementos sean los números comprendidos entre
--n y 1 (incluidos). Si el número es inferior a 1, devuelve la lista vacía.
cuentaRegresiva 0 = []
cuentaRegresiva n = n : cuentaRegresiva (n-1)

--3. 
repetir :: Int -> a -> [a]
--Dado un número n y un elemento e devuelve una lista en la que el elemento e repite n veces.
repetir 0 _ = []
repetir n x = x : repetir (n-1) x

--4. 
losPrimeros :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista con los n primeros elementos de xs.
--Si la lista es vacía, devuelve una lista vacía.
losPrimeros 0 _ = []
losPrimeros _ [] = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

--5. 
sinLosPrimeros :: Int -> [a] -> [a]
--Dados un número n y una lista xs, devuelve una lista sin los primeros n elementos de lista
--recibida. Si n es cero, devuelve la lista completa.
sinLosPrimeros _ [] = []
sinLosPrimeros 0 xs = xs
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs 


--3. Registros

--1. Denir el tipo de dato Persona, como un nombre y la edad de la persona. Realizar las siguientes funciones:

data Persona = 
    P String Int
    --Nombre Edad
    deriving Show

edad :: Persona -> Int
edad (P n e) = e

mayoresA :: Int -> [Persona] -> [Persona]
--Dados una edad y una lista de personas devuelve a las personas mayores a esa edad.
mayoresA _ [] = []
mayoresA 0 ps = ps
mayoresA n (p:ps) = if ((edad p) > n )
                    then p : mayoresA n ps
                    else mayoresA n ps


promedioEdad :: [Persona] -> Int
--Dada una lista de personas devuelve el promedio de edad entre esas personas. Precondición: la lista al menos posee una persona.
promedioEdad ps =  div (sumatoria (edades ps)) (longitud ps)

edades :: [Persona] -> [Int]
edades [] = []
edades (p:ps)= (edad p) : edades ps

elMasViejo :: [Persona] -> Persona
--Dada una lista de personas devuelve la persona más vieja de la lista. Precondición: la
--lista al menos posee una persona.
elMasViejo [p] = p
elMasViejo (p:ps) = if ((edad p) > (edad (elMasViejo ps)))
                    then p 
                    else elMasViejo ps


--2. Modicaremos la representación de Entreador y Pokemon de la práctica anterior de la siguiente manera:

data TipoDePokemon = Agua | Fuego | Planta

data Pokemon = ConsPokemon TipoDePokemon Int

data Entrenador = ConsEntrenador String [Pokemon]

cantPokemon :: Entrenador -> Int
--Devuelve la cantidad de Pokémon que posee el entrenador.
cantPokemon (ConsEntrenador n ps) = longitud ps

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
--Devuelve la cantidad de Pokémon de determinado tipo que posee el entrenador.
cantPokemonDe 

--cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
--Dados dos entrenadores, indica la cantidad de Pokemon de cierto tipo, que le ganarían a los Pokemon del segundo entrenador.
--esMaestroPokemon :: Entrenador -> Bool
--Dado un entrenador, devuelve True si posee al menos un Pokémon de cada tipo posible.


{-




3. El tipo de dato Rol representa los roles (desarollo o management) de empleados IT dentro
de una empresa de software, junto al proyecto en el que se encuentran. Así, una empresa es
una lista de personas con diferente rol. La denición es la siguiente:
data Seniority = Junior | SemiSenior | Senior
data Proyecto = ConsProyecto String
data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
data Empresa = ConsEmpresa [Rol]
Denir las siguientes funciones sobre el tipo Empresa:
proyectos :: Empresa -> [Proyecto]
Dada una empresa denota la lista de proyectos en los que trabaja, sin elementos repetidos.
losDevSenior :: Empresa -> [Proyecto] -> Int
Dada una empresa indica la cantidad de desarrolladores senior que posee, que pertecen
además a los proyectos dados por parámetro.
cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
Indica la cantidad de empleados que trabajan en alguno de los proyectos dados.
asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
Devuelve una lista de pares que representa a los proyectos (sin repetir) junto con su
cantidad de personas involucradas.


-}