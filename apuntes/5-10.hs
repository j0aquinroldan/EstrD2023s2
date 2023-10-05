type SectorID = int
type CUIL = Int  
data Empresa = ConsE 
   (Map SectorID (set Empleado))
   (Map CUIL Empleado )


{-
INV REP : 
    -en el segundo map cada clave esta asociada con dicha clave como cuil.
    - todo sectorId asociado a un empleado ya sea parte de un conjunto valor del primer map o valor del segundo map 
        tiene que estar en el primer map como clave.
    - todo sector id del primer map tiene que tener asociado un conjunto con dichos empleados que tiene dicho sector asociado
    - todo empleado que conforma algun set valor del primer map tiene que se valor del segundo map. 
    -
-}

{

4000 => < (E 1005 <4000>)>
4004 => < (E 1005 <4004>)>
4000 => < (E 1005 <4000>)>

}


{

1005 => E 1005 <4000>
1000 => E 1005 <4000>
}


--prec el sector no existe
agregarSector :: SectorID -> Empresa -> Empresa
agregarSector s (E sxe cxe) =
    case lookupM s sex of 
        Just _ -> error "el sector ya esta registrado"
        _ -> 