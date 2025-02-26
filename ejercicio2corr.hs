-- 1. Definir una función que devuelva el mayor de 2 números
mayorDeDos x y = if x > y then x 
                 else y

-- 2. Definir una función que reciba 3 números y devuelva el mayor
mayorDeTres x y z = if x > y && x > z then x else if y > z then y else z
-- 3. Definir una función que reciba 4 números y devuelva el mayor
mayorDeCuatro w x y z = if x > y && x > z && x>w then x
                        else if w > y && w > z then w
                        else if y > z then y
                        else z
-- 10. Función para determinar el estado académico
calificar p1 p2 fi si = if p1+p2+fi+si == 0 then "abandono"
                        else if(p1+p2) > 51 then "aprobado"
                        else if fi>51 then "aprobado"
                        else if si > 51 then "aprobado"
                        else "reprobado"

-- 11. Definir una función que reciba 2 fechas y devuelva la fecha mayor

fechaMayor (d1,m1,a1) (d2,m2,a2) = 
    if a1 > a2 then 
        (d1,m1,a1) 
    else 
        if a1 < a2 then 
            (d2,m2,a2)
        else 
            if m1 > m2 then 
                (d1,m1,a1) 
            else 
                if m1 < m2 then 
                    (d2,m2,a2)
                else 
                    if d1 > d2 then 
                        (d1,m1,a1) 
                    else (d2,m2,a2)   