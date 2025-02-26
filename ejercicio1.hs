--Definir una función que reciba el lado de un cuadrado y devuelva su área.
areaCuadrado x = x * x
--Definir una función que reciba la base y la altura de un rectángulo y devuelva su área y su perímetro.
areaRectangulo x y = (x*y, 2*x + 2*y)
--Definir una función que reciba 2 números y devuelva verdad si el primero es mayor que el segundo.
primeroMayorQueSegundo x y = x > y
--Definir una función que reciba un número y retorne verdad si este es múltiplo de 2.
esMultiploDeDos x = even (round x)
-- Definir una función que reciba un número y devuelva verdad si este es múltiplo de 2 y de 3 al mismo tiempo.
esMultiploDeDosYTres x = even x && (mod x 3 == 0)
--Definir una función que reciba un número y lo devuelva elevado a la potencia 3.
potenciarCubo x = x ^ 3
-- Definir funciones que reciban un número y lo devuelvan elevado a la potencia4,8,10,32.
potencias x = (x ^ 4, x ^ 8, x ^ 10, x ^ 32)
--Definir una función que reciba dos números y una función de orden y devuelva verdad si los números obedecen a la función de orden, falso en otro caso.
ordenCorrecto :: (a -> a -> Bool) -> a -> a -> Bool
ordenCorrecto f x y = f x y
