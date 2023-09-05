contarProy :: [Rol] -> [(Poroyecto, Int)]
contarProy [] = []
contarProy (r:rs) = SumarProy r ( contarProy rs)

SumarProy :: Rol -> [(Proyecto, Int)]-> [(Proyecto, Int)]
SumarProy r [] = (Proy r, 1) : []
SumarProy r ((p,n):ps) = if trabajaEn r p 
                         then (p,n+1) : ps 
                         else (p,n) : SumarProy r ps  