-- A01064725
-- A00816446

-- Importe de libreria para utilidades
import Data.List

{-
	Funcion que dada una lista de registros calcula el promedio de cada alumno.
	La entrada es en forma de tripletas que contienen en orden: matrcula, nombre y una lista de 
	calificaciones

	Se utilzian los constraints de Real y Fractional para manejar entradas mas genericas
-}

promedio :: (Real a, Fractional b) => [(Integer,[Char],[a])] -> [(Integer,b)]
-- Cuando se tiene la lista vacia se regresa la lista vacia
promedio [] = []
-- Si no esta vacia tomamos los valores que necesitamos, matricula, calificaciones y el resto de la lista
promedio ((mat,_,parcialista):resto) = 
    {- 
        Se regresa una tupla que tiene la matricula y el promedio de las calificaciones, se llama la 
        funcion de promdio con el resto para seguir
    -}
    (mat, (realToFrac(sum parcialista) / genericLength parcialista)) : promedio resto
    {-
        Para calcular el promedop sumamos las calificaciones y las convertimos a numero fraccionario, 
        despues de ello lo dividimos entre la longitud de la lista de calificaciones
    -}


-- Declaramos el tipo de dato de arbol binario
data AB e = N (AB e) e (AB e) | AV deriving Show

-- Declaramos un arbol para pruebas
arbol = N (N AV 2 AV) 1 (N (N AV 4 AV) 3 (N AV 5 AV))



-- Funcion que dado un arbol binario de entrada regresa los nodos hoja del mismo.
hojas :: (AB e) -> [e]
-- En caso de que sea un arbol vacio regresmaos la lista vacia
hojas AV = []
-- Dado el caso que sea un nodo hoja se regresa la lista con valor e
hojas (N AV e AV) = [e]
-- Dado un nodo cualquiera llama recursivamente a la funcion hojas con cada subarbol
hojas (N i e d) =  (hojas i) ++ (hojas d)



-- Declaramos el tipo de dato
data LA e = L [LA e] | E e deriving Show

lista = L [E 1,L [E 2,E 3],E 4]

{-
    Funcion que recibe una lista posiblemente inbricada y cambiara los numeros pares
    de elementos por 0 y los impares con 1.
-}
binariza :: (LA Integer) -> (LA Integer)
-- Hace match con las listas para procesarlas
binariza (L x) =  (L (auxbinariza x))
-- Hace match con los elementos y los procesa
binariza (E x) = if(even x)
                    then (E 1)
                    else (E 0)

{-
    Funcion auxiliar de binariza para romper listas en elementos para su
    correcto procesamiento.
-}
auxbinariza :: [LA Integer] -> [LA Integer]
-- En caso de que la lista sea vacia regresamos vacio
auxbinariza [] = []
{- 
    Se procesa el primer elemento de la lista por binariza y el resto se procesa en 
    auxbinariza lo que permite reconstruir la lista.
-}
auxbinariza (x:xs) = (binariza x):(auxbinariza xs)