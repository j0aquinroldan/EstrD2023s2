--NUMERO ENTEROS 1) 
--a)
sucesor :: Int->Int
sucesor n = n + 1

-- b)
sumar :: Int -> Int -> Int
sumar n m = n+m

--c)
divisionYResto :: Int -> Int -> (Int,Int)
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
negar False = True

--b
implica :: Bool -> Bool -> Bool 
implica _ True = True
implica _ _ = False

--c
yTambien :: Bool -> Bool -> Bool
yTambien False _ = False
yTambien _ False = False
yTambien _ _ = False

--d
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True
oBien _ _ = False

{-
oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ True = True
oBien _ _ = False
-}

-- 4 REGISTROS
data Persona = 
    P String Int
    --Nombre Edad
    deriving Show

nombre :: Persona -> String
nombre (P n e) = n

-- esta bien pero consultar como declarar variables

edad :: Persona -> Int
edad (P n e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e+1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nombreNuevo (P n e) = P nombreNuevo e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra  (P n1 e1) (P n2 e2) =
                 if e1 > e2
                    then True
                    else False

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
tipo (Pokemon t e)  = t 

superaA :: Pokemon -> Pokemon -> Bool
superaA (Pokemon t1 e1) (Pokemon t2 e2) = superaATipo t1 t2

superaATipo :: TipoPokemon-> TipoPokemon -> Bool
superaATipo Agua Fuego = True
superaATipo Fuego Planta = True
superaATipo Planta Agua = True
superaATipo _ _ = False


