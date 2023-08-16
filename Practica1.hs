--NUMERO ENTEROS 1) 
--a)
sucesor :: Int->Int
sucesor n = n + 1

-- b)
sumar :: Int -> Int -> Int
sumar n m = n + m

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
vieneDespues Lunes _ = False
vieneDespues _ Lunes  = True
vieneDespues Martes _ = False
vieneDespues Miercoles _ = False
vieneDespues Jueves _ = False
vieneDespues Viernes _ = False
vieneDespues Sabado _ = False
vieneDespues Domingo _ = True
vieneDespues _ Domingo = False

--d
estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True