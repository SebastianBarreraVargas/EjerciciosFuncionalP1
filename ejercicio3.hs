--EJERCICIO 3 (Resolver usando Distinción de casos)
--1. Definir una función que reciba 4 número y devuelva el mayor.
-- Por combinación.
maxComb :: (Ord a) => a -> a -> a -> a -> a
maxComb a b c d = max a (max b (max c d))
-- Por distinción de casos.
maxCas a b c d | a > b && a > c && a > d = a
               | b > c && b > d = b
               | c > d = c
               | otherwise = d
--Definir una función que reciba una nota y devuelva el mensaje “Aprobado” o “Reprobado”.
eval :: (Ord a, Num a) => a -> String
eval x | x >= 51 = "apr"
       | otherwise = "rep"
--Definir una función que reciba una nota y devuelva el mensaje “Excelente“ si la nota
--esta entre 90-100, “Bien” si esta entre 70-89, “Regular” si esta entre 51-69 y mal si esta
--entre 0-50.
evalS :: (Ord a, Num a) => a -> String
evalS a | a >= 90 = "Excelente"
       | a < 90 && a >= 70 = "Bien"
       | a < 70 && a >= 51 = "Regular"
       | a < 51 && a >= 0 = "Mal"
       | otherwise = "Introduzca una nota valida"
--Definir una función que reciba como argumentos las notas de primer parcial, segundo
--parcial, final y segunda instancia y retorne el mensaje aprobado o reprobado, según el
--caso.
evalN :: (Fractional a, Ord a) => a -> a-> a -> a-> String 
evalN p1 p2 f si | (p1 + p2)/2 >= 51 = "aprobado"
                 | f >= 51 = "aprobado"
                 | si >= 51 = "aprobado"
                 | otherwise = "reprobado" 
--Definir una función que reciba 16 números y retorne el mayor
--Yo no queria hacerlo asi pero tiene pinta que segun lo que se pidio debiamos hacerlo asi xddd
max16 a b c d e f g h i j k l m n o p
    | a > b && a > c && a > d && a > e && a > f && a > g && a > h && a > i && a > j && a > k && a > l && a > m && a > n && a > o && a > p = a
    | b > c && b > d && b > e && b > f && b > g && b > h && b > i && b > j && b > k && b > l && b > m && b > n && b > o && b > p = b
    | c > d && c > e && c > f && c > g && c > h && c > i && c > j && c > k && c > l && c > m && c > n && c > o && c > p = c
    | d > e && d > f && d > g && d > h && d > i && d > j && d > k && d > l && d > m && d > n && d > o && d > p = d
    | e > f && e > g && e > h && e > i && e > j && e > k && e > l && e > m && e > n && e > o && e > p = e
    | f > g && f > h && f > i && f > j && f > k && f > l && f > m && f > n && f > o && f > p = f
    | g > h && g > i && g > j && g > k && g > l && g > m && g > n && g > o && g > p = g
    | h > i && h > j && h > k && h > l && h > m && h > n && h > o && h > p = h
    | i > j && i > k && i > l && i > m && i > n && i > o && i > p = i
    | j > k && j > l && j > m && j > n && j > o && j > p = j
    | k > l && k > m && k > n && k > o && k > p = k
    | l > m && l > n && l > o && l > p = l
    | m > n && m > o && m > p = m
    | n > o && n > p = n
    | o > p = o
    | otherwise = p
--Definir una función que reciba un quebrado y devuelva verdad si este es mayor que 1 y falso en otro caso
porcentaje :: Rational -> Bool
porcentaje x 
   | x > 1 = True
   | otherwise = False
--Definir una función que reciba 2 fechas y devuelva la fecha mayor
dosFechas (d1,m1,a1) (d2,m2,a2) | a1 < a2 = (d1,m1,a1)
                                | m1 > m2 = (d1,m1,a1)
                                | d1 > d1 = (d1,m1,a1)
                                | otherwise = (d2,m2,a2)
--Definir una función que reciba 2 fechas y devuelva los años transcurridos
añosEntre :: (Int,Int,Int) -> (Int,Int,Int) -> Int
añosEntre (d,m,a) (d1,m1,a1) 
    | (a, m, d) > (a1, m1, d1) = añosEntre (d1, m1, a1) (d, m, a) 
    | (m, d) > (m1, d1) = a1 - a - 1  
    | otherwise = a1 - a
--Definir una función que reciba 2 fechas y devuelva los meses transcurridos    
mesesEntre :: (Int, Int, Int) -> (Int, Int, Int) -> Int
mesesEntre (d,m,a) (d1,m1,a1)
    | (a, m, d) > (a1, m1, d1) = mesesEntre (d1, m1, a1) (d, m, a)
    | d > d1 = (a1 - a) * 12 + (m1 - m) - 1 
    | otherwise = (a1 - a) * 12 + (m1 - m)          
--Definir una función que reciba 2 fechas y devuelva los días transcurridos
diasEntre :: (Int,Int,Int) -> (Int,Int,Int) -> Int
diasEntre (d,m,a) (d1,m1,a1) | (d,m,a) == (d1,m1,a1) = 0
                             | (a, m, d) > (a1, m1, d1) = diasEntre (d1, m1, a1) (d, m, a)
                             | a==a1 && m==m1 = d1-d
                             | a==a1 = diasMes (d,m,a) (d1,m1,a1)
                             | otherwise = diasMes (d,m,a) (d1,m1,a1)
diasMes (d,m,a) (d1,m1,a1) | m == 12 = 31 - d + diasEntre (0, 1, a+1) (d1, m1, a1)
                           | tipoMes (d,m,a) == 31 = 31-d + diasEntre (0,m+1,a) (d1,m1,a1)
                           | tipoMes (d,m,a) == 30 = 30-d + diasEntre (0,m+1,a) (d1,m1,a1)
                           | tipoMes (d,m,a) == 28 = 28-d + diasEntre (0,m+1,a) (d1,m1,a1)
                           | tipoMes (d,m,a) == 29 = 29-d + diasEntre (0,m+1,a) (d1,m1,a1)
tipoMes :: (Int,Int,Int) -> Int
tipoMes (d,m,a) | m == 1 || m == 3 || m == 5 || m == 7 || m == 8 || m == 10 || m == 12 = 31
                | m == 4 || m == 6 || m == 9 || m == 11 = 30
                | otherwise = if (a `mod` 4 == 0 && a `mod` 100 /= 0) || (a `mod` 400 == 0) then 29 else 28

--Definir una función que reciba 2 fechas y devuelva los días, meses y años transcurridos
todoEntre (d,m,a) (d1,m1,a1) = (diasEntre (d,m,a) (d1,m1,a1),mesesEntre (d,m,a) (d1,m1,a1), añosEntre (d,m,a) (d1,m1,a1)) 

--Definir una función que reciba un instante (fecha, hora) e incremente el instante en 1 segundo.

incSeg :: (Int, Int, Int) -> (Int, Int, Int) -> ((Int, Int, Int),(Int, Int, Int))
incSeg (d,m,a) (h,min,s) |(23,59,59) == (h,min,s) && tipoMes (d,m,a) == 30 && d==30 = ((1,m+1,a),(0,0,1))
                         |(23,59,59) == (h,min,s) && m==12 && d==31 = ((1,1,a+1),(0,0,1))
                         |(23,59,59) == (h,min,s) && tipoMes (d,m,a) == 31 && d==31 = ((1,m+1,a),(0,0,1))
                         |(23,59,59) == (h,min,s) && tipoMes (d,m,a) == 28 && d==28 = ((1,m+1,a),(0,0,1))
                         |(23,59,59) == (h,min,s) && tipoMes (d,m,a) == 29 && d==29 = ((1,m+1,a),(0,0,1))
                         |min==59 && s == 59 = ((d,m,a),(h+1,0,1))
                         |s==59 = ((d,m,a),(h,min+1,1))
                         |otherwise = ((d,m,a),(h,min,s+1))
                            