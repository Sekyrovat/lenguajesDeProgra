-- A01064725
-- A00816446

{-
    Funcion que recibe como entrada una lista de enteros y cuenta el total de 
    numero positivos y negativos, y regresa la cuenta de cada total en una
    tupla que sigue la forma (#negativos, #positivos)
-}
cuenta_signo :: (Num x, Ord x) => [x] -> (Int, Int)
-- Nuestro caso base regresa la tupla (0,0)
cuenta_signo [] = (0, 0)
-- En caso de que no proseguimos
cuenta_signo (x:xs)
                    {-
                        Aqui vamos a proceder a ver si x es mayor o menor a 0.
                        En caso de que si llamamos a la funcion auxiliar con el valor
                        de identificador de la tupla correspondiente.

                        Esto sirve debido a que el caso base genera la tupla, y todas
                        las demas iteracioens van a operar sobre la tupla.
                    -}
                    | x < 0 = cuenta_signo_aux (cuenta_signo xs) 'x'
                    | x > 0 = cuenta_signo_aux (cuenta_signo xs) 'y'
                    | otherwise = cuenta_signo xs

-- Funcion auxiliar para cuenta signo, recibe una tupla y el ligar donde hacer la suma
cuenta_signo_aux :: (Int,Int) -> Char -> (Int, Int)
cuenta_signo_aux (x, y) char
                        -- Si recibe char x suma en negativos
                        | char == 'x' = (x + 1, y)
                        -- Si recibe char y suma en los positivos 
                        | char == 'y' = (x, y + 1)
                        | otherwise = (x, y)

{-
    Funcion que dada una lista de enteros genera una tupla que contiene dos listas de
    enteros, con los vcalores menores a n y otra con los valroes mayores a n 
    incluyendo n.
    Para realziarlo utiliza comprension de listas
-}
separa :: (Num x, Ord x) => x -> [x] -> ([x],[x])
separa _ [] = ([], [])
separa n xs = ([x | x <- xs, x < n], [x | x <- xs, x >= n])

-- Funcion que calcula el factorial de un numero dado
factorial :: Integer -> Integer
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x - 1)

{-
    Funcion que calcula la funcion exponencial a partir de n terminos y el factorial
    correspondiente.
-}
fexp :: (Fractional x) => x -> Int -> x
{-
    EL take n nos sirve para acotar la lista al numero que buscamos, y a cada numero de 
    dicha lista se le aplica la funcion lambda para obtener la aproximacion de e a la x
    utilizando dicha cantidad de elemento de la serie infinita.
-}
fexp x n = sum (map (\n -> (x ^ n) / (fromIntegral (factorial n)) ) (take n [0..]))
