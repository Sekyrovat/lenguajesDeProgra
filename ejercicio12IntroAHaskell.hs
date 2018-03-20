-- A01064725
-- A00816446

-- Caso inicial que recibe los 4 parametros
menor :: Integer -> Integer -> Integer -> Integer -> Integer
menor a b c d
    -- Llama funciones en secuencia para obtener el menor de ellos
    | a < b = menor3 a c d
    | b < a = menor3 b c d

-- Funcion que regresa el menor de 3 numero por medio de un auxiliar
menor3 :: Integer -> Integer -> Integer  -> Integer
menor3 a b c
    | a < b = menor2 a c
    | b < a = menor2 b c

-- Funcion que regresa el menor de 2 numeros
menor2 :: Integer -> Integer -> Integer
menor2 a b
    | a < b = a
    | b < a = b 

{- 
    Funcion que dado un argumento inicial genera una lista con la 
    secuencia de 0 al parametro de entrada
-}
secuencia :: Integer -> [Integer]
-- El caso default regresa la lista con 0
secuencia 0 = [0]
-- Nuestro caso recursivo genera la lista por medio de appends
secuencia n = secuencia(n - 1)++[n]


{- 
    Funcion que regresa la lista con los numeros negativos de una 
    lista de entrada
-}    
negativos :: [Integer] -> [Integer]
negativos [] = []
negativos (x:xs) = if x < 0 then x:negativos xs
                        else negativos xs


-- Fucnion que calcula la suma de los numeros pares menores a un umbral
sumapares :: [Integer] -> Integer -> Integer
-- Caso final que rompe la recursividad
sumapares [] b = 0
-- Aqui vemos cada numero que tiene la lista
sumapares (a:xs) b = 
                    -- Comparamos el numero con el umbral
                    if a < b
                        -- Tras ello vemos si el numero es par
                        then if even a
                                -- Si si lo agregamos a la suma
                                then a + sumapares xs b
                                {- 
                                    Si no, llamamos a la funcion con 
                                    el resto de la lista
                                -}
                                else sumapares xs b
                        else
                            {- 
                                Si no, llamamos a la funcion con 
                                el resto de la lista
                            -}
                            sumapares xs b