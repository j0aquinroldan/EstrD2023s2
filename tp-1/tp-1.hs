--NUMERO ENTEROS 1) 
--a)
sucesor :: Int->Int
sucesor n = n + 1

-- b)
sumar :: Int -> Int -> Int
sumar n m = n+m

--c)
divisionYResto :: Int -> Int -> (Int,Int)
--PRECONDICION: el parametro m no puede ser 0
divisionYResto n m = (div n m, mod n m)

--d)
maxDelPar :: (Int,Int) -> Int
maxDelPar (n, m) = if n > m 
                  then n 
                  else m 

--NUMERO ENTEROS 2)
{-
 sumar (sucesor 5) (maxDelPar(divisionYResto 4 1 ))

sucesor (sumar 5(maxDelPar(divisionYResto 4 1 )))

maxDelPar (divisionYResto  (sucesor 9)(sumar (-1) 2 ))

sucesor (maxDelPar (divisionYResto  (sucesor 8)(sumar 0 1 )))
-}


-- TIPOS ENUMERATIVOS 1)

data Dir = Norte | Este | Sur | Oeste
 deriving Show

--a)

opuestoDir :: Dir -> Dir
opuestoDir Norte = Sur
opuestoDir Este = Oeste
opuestoDir Sur = Norte
opuestoDir Oeste = Este 

-- b)
iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur  = True
iguales Oeste Oeste = True
iguales _ _ = False


--c)


siguienteDir :: Dir -> Dir
siguienteDir Norte = Este
siguienteDir Este = Sur
siguienteDir Sur = Oeste
siguienteDir Oeste = Norte 

{- 
caso donde Oeste no tiene siguiente
PRECONDICION: Oeste no tiene siguiente Dir
(la funcion es parcial porque no funciona sobre todas las variantes de Dir)
-}
siguienteDir' :: Dir -> Dir
siguienteDir' Norte = Este
siguienteDir' Este = Sur
siguienteDir' Sur = Oeste


-- TIPOS ENUMERATIVOS 2)

data DiaDeSemana = Lunes | Martes | Miercoles | Jueves | Viernes | Sabado | Domingo
 deriving Show


--a
primeroYUltimoDia :: (DiaDeSemana, DiaDeSemana)
primeroYUltimoDia = (Lunes, Domingo)


--b
empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False 

--c
vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues d1 d2 = 
   numeroDia d1 > numeroDia d2 
   

numeroDia :: DiaDeSemana -> Int
numeroDia Lunes = 1
numeroDia Martes = 2
numeroDia Miercoles = 3
numeroDia Jueves = 4
numeroDia Viernes = 5
numeroDia Sabado = 6
numeroDia Domingo = 7

--d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True


-- TIPOS ENUMERATIVOS 2)
--a
negar :: Bool -> Bool
negar True = False
negar _ = True

--b
implica :: Bool -> Bool -> Bool 
implica True b = b
implica False _ = True

--c
yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

--d
oBien :: Bool -> Bool -> Bool
oBien False False= False
oBien _ _ = True


-- 4 REGISTROS
data Persona = 
    P String Int
    --Nombre Edad
    deriving Show

nombre :: Persona -> String
nombre (P n _) = n


edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1) 

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombreNuevo (P n e) = P nombreNuevo e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra  p1 p2 = edad p1 > edad p2
                    
                    
laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor  p1 p2 =
                 if esMayorQueLaOtra p1 p2
                    then p1
                    else p2


-- 4 REGISTROS 2
data Pokemon = 
    Pokemon TipoPokemon Int
    --      tipo        porcentaje energia
    deriving Show

data TipoPokemon = Agua | Fuego | Planta
   deriving Show

data Entrenador = 
    Ent String Pokemon Pokemon
    -- nombre  pokemon1 pokemon2
    deriving Show


tipo :: Pokemon -> TipoPokemon
tipo (Pokemon t _)  = t 

superaA :: Pokemon -> Pokemon -> Bool
superaA (Pokemon t1 _) (Pokemon t2 _) = superaATipo t1 t2

superaATipo :: TipoPokemon-> TipoPokemon -> Bool
superaATipo Agua Fuego = True
superaATipo Fuego Planta = True
superaATipo Planta Agua = True
superaATipo _ _ = False

--d
cantidadDePokemonDe :: TipoPokemon -> Entrenador -> Int
cantidadDePokemonDe tipoPok (Ent _ p1 p2) =  fromEnum (coincideTipo tipoPok p1)
                                        + fromEnum (coincideTipo tipoPok p2) 

coincideTipo :: TipoPokemon->Pokemon-> Bool
coincideTipo  Agua   (Pokemon Agua _)   = True
coincideTipo  Fuego  (Pokemon Fuego _)  = True
coincideTipo  Planta (Pokemon Planta _) = True
coincideTipo _ _ = False

--c
juntarPokemon :: (Entrenador, Entrenador) -> [Pokemon]
juntarPokemon (ent1, ent2) = pokemonesDe ent1 ++ pokemonesDe ent2

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (Ent _ p1 p2) = [p1, p2 ]

-- 5. FUNCIONES POLIMORFICAS

-- 1 a)

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a,b) -> (b, a)
swap (x,y) = (y,x)

{-
¾Por qué existen dos variables de tipo diferentes?
porque las tuplas permiten elementos de distinto tipo, ademas de que de esta forma se pueden 
utilizar justamente argumentos de distinto tipo, la funcion no esta atada aa un solo tipo.
-}

--2
-- Las funciones son polimorficas porque no estan sujetas a un tipo especifico 

-- PATTERN MATCHING SOBRE LISTAS

--2
estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False 

--3
elPrimero :: [a] -> a
-- PRECONDICION : la lista no debe ser vacia
elPrimero (x : _) = x
elPrimero _ =  error "no se puede usar con []"


--4
sinElPrimero :: [a] -> [a]
-- PRECONDICION : la lista no debe ser vacia
sinElPrimero ( _ : xs) = (xs)
sinElPrimero _ =  error "no se puede usar con []"

--5
splitHead :: [a] -> (a, [a])
-- PRECONDICION : la lista no debe ser vacia
splitHead xs = (elPrimero xs , sinElPrimero xs)