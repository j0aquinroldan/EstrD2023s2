-- 3 


{-
INV REP 
 * cada clave del primer map esta asociado a una foto con dicha clave como cuif
 * cada vehiculo presente en las fotos valor del primer map debe ser clave del segundo asociado a un set que incluye al resto de los vehiculos del set mencionado anteriormente
 * cada elemento de los sets valor del segundo map debe estar presente en una foto del primer map con la clave del set nombrado anteriormente
 

 -- para opocion con maximo, la foto almacenada es la foto con mas vehiculos de todo el registro, a excepcion de un registro sin fotos para cuyo caso es Nothing
-}

data RV = MkRV (Map CUIF Foto)
               (Map Vehiculo (Set Vehiculo))
               Maybe Foto -- se agrega esto 

-- usar en agregarFoto
maxFoto:: Foto -> Maybe Foto -> Maybe Foto
maxFoto f Nothing = Just f 
maxFoto (MfK c1 vs1) (Just f2) =
let MkF c2 vs2 = fs in
    if size vs1 > sizeS vs2
        then Just  (MfK c1 vs1)
        else Just (MfK c2 vs2)

masCargada :: RV -> Maybe CUIF 
masCargada (MkRV cxf vxv mf)= 
    case mf of 
        Just mf2 -> let (MkF c _ ) = mf2 in Just c
        Nothing -> Nothing