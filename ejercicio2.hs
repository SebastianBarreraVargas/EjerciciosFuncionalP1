--Definir una función que devuelva el mayor de 2 números
mayor x y = max x y 
--Definir una función que reciba 3 números y devuelva el mayor
mayorTres x y z = max x(max y z)
--Definir una función que reciba 4 números y devuelva el mayor
mayorCuatro w x y z = max w (max x (max y z))
--Definir una función que reciba dos exámenes parciales, un final y una segunda instancia y devuelva el mensaje “Aprobado”, “Reprobado” o “Abandono” según el caso.
aprobadoMateria :: Float -> Float -> Float -> Float -> String
aprobadoMateria 0 0 0 0 = "abandono"
aprobadoMateria p1 p2 final si 
    | ((p1 + p2) / 2) >= 51 = "aprobado"
    | final >= 51 = "aprobado"
    | si >= 51 = "aprobado"    
    |otherwise = "reprobado"
--Definir una función que reciba 2 fechas y devuelva la fecha mayor
fechaMayor (d1,m1,a1) (d2,m2,a2)
    | a1 > a2 = (d1,m1,a1)
    | a1 < a2 = (d2,m2,a2)
    | m1 > m2 = (d1,m1,a1)
    | m1 < m2 = (d2,m2,a2)
    | d1 > d2 = (d1,m1,a1)
    | d1 < d2 = (d2,m2,a2)
    | otherwise = (d2,m2,a2)