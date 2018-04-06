-- A01064725
-- A00816446

{-# LANGUAGE ParallelListComp #-}

{-
    Funcion que dado un grupo de 5 argumentos regresa el valor
    de la moda.
-}
moda :: (Num x, Eq x) => x -> x -> x -> x -> x -> x
moda a b c d e
    {-
        Primero comparamos la a con b, en caso de que cumpla
        si concuerda con alguno de los otros tres numeros,
        al llegar a una tercera repeticion se vuelve la moda.
        Si no, buscamos si c, d y e son iguales,
        en cuyo caso estos valores serian la moda.
        Si no a es la primera o unica repeticion de 2.
    -}
    | a == b =
        if a == c || a == d || a == e then a
        else if  c == d && c == e then c
        else a
    {-
        Procedimiento similar al anterior pero con una comparacion
        menos ya que a y b no son iguales
    -}
    | b == c =
        if b == d || b == e then b
        else if a == d && a == e then a
        else b
    {-
        Nuevamente se presenta un patron similar, pero 
        ya sabemos que a es diferente de b y b es diferente
        de c
    -}
    | a == c =
        if a == d || a == e then a
        else if b == d && b == e then b
        else a
    {-
        Si no se cumplio con lo anterior buscamos por dobles
        ya que el que se repita sera la moda
    -}
    | a == d = a
    | a == e = a
    | b == d = b
    | b == e = b
    | c == d = c
    | c == e = c
    | d == e = d
    | otherwise = a 

{-
    Funcion basica recursiva que dado un entero positivo 
    despliega la tabla de multiplicar del numero
-}
tabla :: Integer -> IO ()
tabla x = putStr $ tabla_aux x 1

tabla_aux :: Integer -> Integer -> [Char]
tabla_aux x 10 = show x ++ " x " ++ show 10 ++ " = "
    ++ show (x * 10) ++ "\n"
tabla_aux x n = show x ++ " x " ++ show n ++ " = " ++ show (x * n)
    ++ "\n" ++ tabla_aux x (n + 1)

{-
    FUncion recursiva con listas que dadas dos listas
    regresa una sublista de sublistas que contiene los elementos
    de la segunda lista que son menores que cada elemento de la
    primera lista.
    Se genera una sublista por cada elemento de de la 
    primera lista
-}
menores :: [Integer] -> [Integer] -> [[Integer]]
-- En caso de ser una lista vacia regresamos la lista vacia
    menores [] _ = []
{- 
    Llamamos a la funcion auxiliar que genera una lista por cada x con que se llama.
    Y llamamos de forma recursiva esta misma funcion con el resto de la lista.
    Se debe notar que mandamos a la funcion auxiliar el valor que sera usado para
    comparar con la lista completa.
-}
menores (x:xs) lista2 = (menores_aux x lista2) :
    (menores xs lista2)

menores_aux :: Integer -> [Integer] -> [Integer]
-- En caso de ser una lista vacia regresamos la lista vacia
menores_aux _ [] = []
{-
    Aqui tomamos todo elemento y de la lista y lo comparamos con el elemento
    x de la primera lista que se nos dio como parametro.
    Y llamamos de forma recursicva a la funcion con el resto de la lista.
    Solo si y es menor a x se agregara a la lista de salida.
-}
menores_aux x (y:ys) = if x > y then y : (menores_aux x ys)
    else (menores_aux x ys)

{-
    Saltos que dados 4 enteros (n, i, s1, s2) regresa una lista
    de n elementos iniciando en i y se obtienen los siguientes
    intercalando entre s1 y s2 los siguiente numeros
-}
saltos :: Integer -> Integer -> Integer -> Integer -> [Integer]
saltos 0 _ _ _ = []
saltos n i s1 s2 = saltos_s1 n i s1 s2

-- FUncion auxiliar que realiza el salto con S1
saltos_s1 :: Integer -> Integer -> Integer -> Integer -> [Integer]
-- Cuando n es igual a 0 entonces regresamos la lista vacia.
saltos_s1 0 _ _ _ = []
{-
    Si n no fue 0 entonces agregamos el valor de i a la lista, y llamamos
    a la funcion auxiliar saltos_s2 que con la nueva i que fue modificada
    por S1, para que saltos_s2 se encarge de agregar el salto a la lista
    de salida si es valido.
-}
saltos_s1 n i s1 s2 = i : (saltos_s2 (n - 1) (i + s1) s1 s2)

-- FUncion auxiliar que realiza el salto con S2
saltos_s2 :: Integer -> Integer -> Integer -> Integer -> [Integer]
-- Cuando n es igual a 0 entonces regresamos la lista vacia.
saltos_s2 0 _ _ _ = []
{-
    Si n no fue 0 entonces agregamos el valor de i a la lista, y llamamos
    a la funcion auxiliar saltos_S1 que con la nueva i que fue modificada
    por S1, para que saltos_S1 se encarge de agregar el salto a la lista
    de salida si es valido.
-}
saltos_s2 n i s1 s2 = i : (saltos_s1 (n - 1) (i + s2) s1 s2)


-- Definicion del tipo de dato para Arbol Binario.
data AB t = A (AB t) t (AB t) | V deriving Show

-- se llama ab por que AB no es un nombre valido
ab = A (A (A V 2 V)
           5
           (A V 7 V))
       8
       (A V
          9
          (A (A V 11 V)
             15
             V))
{-
    LA funcion se encarga de desplegar la informacion de un AB
    que recive como parametro de entrada en el orden que se
    especifique en la misma entrada.
    Las opciones son:
        prefix
        infix
        postfix (Debido al uso de appends es la menos
                 eficiente)
-}
lista_en_orden :: AB t -> [Char] -> [t]
-- Cuando se recibe el arbol vacio regresmaos la lista vacia
lista_en_orden V _ = []
{- 
    Si se llega aun nodo hoja se despliega y no se hace nada mas
    esto es importante ya que es una de las situaciones que
    rompen la recursion.
-}
lista_en_orden (A V x V) _ = [x]
{-
    En caso de que no se cumpla lo anterior nos basaremos en fix
    que sera la variable que tome el valor del tipo de despliegue
    que se desea.
-}
lista_en_orden (A i x d) fix
    | fix == "prefix" =
        {-
            Empezamos a generar la lista de salida con el valor
            de x del nodo actual y usamos como siguiente elemento
            de la lista lo resultante de llamar la misma funcion
            con el nodo izquierdo, al resultado de esto le
            concatenamos lo resultante de evaluar el arbol derecho
            en la misma funcion.
        -}
        (x : (lista_en_orden i fix)) ++
        (lista_en_orden d fix)
    | fix == "infix" =
        (lista_en_orden i "infix") ++
        (x : (lista_en_orden d "infix"))
    | fix == "postfix" =
        (lista_en_orden i "postfix") ++
        (lista_en_orden d "postfix") ++
        [x]

agrega_abb :: (Num t, Ord t) => AB t -> t -> AB t
agrega_abb V x = A V x V
agrega_abb (A i v d) x
    | x <= v =
        A (agrega_abb i x) v d
    | otherwise =
        A i v (agrega_abb d x)

{-
    Funcion que evaluara si dos listas son disjuntas.
    Para realizar eso aplicara por medio de un map una
    funcion que evaluara si hay algun elemento de la lista
    2 que sea igual al elemento dado de la lista 1.
    En caso de que se cumpla regresara True. 
    Map nos regresara por ende un arreglo de booleanos
    al cual le aplicaremos un or para ver si hay alguno
    igual en cualquier punto, por ultimo negaremos el 
    valor.
-}
g_disjuntas :: (Eq t) => [t] -> [t] -> Bool
g_disjuntas list1 list2 =
    not $ or (map (\n -> any (== n) list2) list1)

{-
    Esta funcion crea por medio de comprehension de listas
    una lista con todas las potencias de 2 de 2 al numero
    que se de como entrada.
    Para realizar dicho funcion se eleva al cuadrado el
    valor de x, donde x es el valor que se vaya generando
    con la lista infinita creciendo en pares de 2 a n.
-}
c_pares2 :: Integer -> [Integer]
c_pares2 n = [x^2 | x <- [2,4..n]]

{-
    Funcion que dada una lista de sublistas de enteros
    regresa el numero de numeros pares por cada sublista.
    Para realizar dicha funcion se realiza un map que realizara
    la siguiente funcion a cada sublista de la lista;
    se realiza un filtro si es o no impar el elemento,
    se obtiene el tamanio de la lista. 
    Lo anterior nos permite obtener el tamanio de cada
    lista que resulta de filtrar los numeros impares.
-}
f_cuentasub :: (Integral x) => [[x]] -> [Int]
f_cuentasub lista = 
    map (\sublista -> length $ filter odd sublista) lista
