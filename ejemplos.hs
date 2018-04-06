-- Calcula la funcion sucesor
-- La funcion sucesor recibe un Integer y genera un Integer
sucesor :: Integer -> Integer
-- La funcion sucesor empata con un argumento y te regresa el argumento mas uno
sucesor x = x + 1

-- Calcula la suma de cuadrados
-- Aqui tenemos la secuencia de Int porque esta currificado
sumaCuadrados :: Int -> Int -> Int
sumaCuadrados x y = x * x + y * y

{-
	Si a la funcion de antes solo le doy un parametro es el de X y se puede llenar esa parte
	Y elimino el primer Int de la segunda parte de la declaracion, que indica los tipos
	Ejemplo de lo anterior
		(lambda(x) . lambda(y). x*x + y*y)2
		lambda(y). 4 + y*y
-}

-- Calcula factorial
factorial :: Int -> Int
-- Esta expresion solo va a empatar si el entero de entrada es 0
-- El orden en que declares es importante, ya que si tuvieramos al reves las siguientes

factorial 0 = 1
factorial n = n * factorial (n - 1)

{-
	Funciones para listas adaptadas a haskell
	Los primitivos que se usan son hd para car y tl para cdr
		Las listas en Haskell van entre []

	Defino la lista con a porque no se que va a entrar en la lista ni que va a recibir, por lo que
	permito el polimorfismo.
			Los tipos deben iniciar con mayuscula por que son constantes
			Elemento que empiezan con minuscula son variables y se asocian con objetos
		Si bien le puse a lo unico que importa de esto es que sea con minuscula, lease las funcioens y 
		parametros deben ir con misnucula al menos como primer letra ya que no son constantes. 
-}
car :: [a] -> a
{-
	Tomamos solo el elemento y no el resto de la lista
	Para que no tome nada del resto de la lista usas el '_'
	para que se vea como una variable anonima
-}
car (x:_) = x

carConError :: [a] -> a
carConError xs = case xs of
    [] -> error "Lista vacia"
    (x:_) -> x

-- El ':' nos delimita la lista como la celda cons

cdr :: [a] -> [a]
cdr (_:xs) = xs


longitud :: [a] -> Int
-- Solo empata con la lista vacia, es como null? en racket
longitud [] = 0
-- Vuelvo a llamar la longitud y me enfoco en el resto de la lista
longitud (_:xs) = 1 + longitud xs

{-
    Los strings son arrays y les puedes hacer lo mismo que a cualquier array
	*Main> "hola"
	"hola"

	*Main> ['h','o','l','a']
	"hola"
-}


-- Inicamos que el segundo parametro es una lista de enteros
posint :: Int -> [Int] -> Int
{-
	La posicion de algo en la lista vacia o de un elemento que no existe
	No voy a desplegar el mensaje sino que llamo a la funcion de error con
	el mensaje a desplegar.
-}
posint _ [] = error "Elemento no encontrado"

posint x (y:lista) = 
    -- Si x igual a y entonces ya llegamos al destino y podemos desplegar un ultimo 1
    if x == y then 1
        else 1 + posint x lista


-- La fucnion putStrLn "Hello World" despliega Hello World


{- 
	Tuplas, van entre () ejem (1,2)
	Cada elemento de la tupla puede ser de distinto tipo
	Pero son de un tamanio definido
	No podemos mezclar tuplas, deben ser del mismo tamanio

    ejemplo = [[750706,"Ramiro Flores",
                [97,85,91]],
                [773454,"Myrna Vazquez",
                [98,75,88]],
                [764435,"Ruben Solis",
                [77,56,80]]]

	[Int] representa una lista de enteros acotados permite manejar [1,2,3]
	Si quiero manejar [[1,2][3,4,5]] lo declaro como [[Int]]

	Con ([Char], (Char, Char)) puedo crear ("hola", ('a','b'))

    [[(Integer,Integer)]] Lista de listas de pares de enteros = [[(1,3),(10,5)], [(25,40),(30,18),(4,90)]]


    Tipo ejemplo :: [(Integer,[Char],[Double])] = Una lista de tuplas que acepta un numero entero, array de chars y una lista de doubles.
-}

promalum :: Integer -> [(Integer,[Char],[Double])]-> Double
--Tiene 2 parametros
promalum _ [] = 0.0
-- Aqui hay 4 parametros
promalum mat1 ((mat2,_,parcialista):resto) = if mat1 == mat2 
                                                then
                                                    -- fromIntegral convierte la longitud de entero a double ya que el lenguaje es de tipos duros
                                                    -- Ya que no podemos mezclar tipos distintos
                                                    sum parcialista / fromIntegral (length parcialista)
                                                else
                                                    promalum mat1 resto

{-
    Una secuencia se puede representar asi [1..4] o [1,2,3,4] y son iguales
    La secuencia con .. es unitara por default.
    Si quiero modificar el paso entre elementos se hace de la siguiente manera [1,4 .. 12] y la diferencia entre el primer elemento (1) 
        y el segundo (4) sera el tamanio de los saltos [1, 4, 7, 10] sin embargo el sig numero seria 13 pero como se pasa del limite no se pone.

    Si quiero que sea decreciente solo tengo que pedirlo [4,1..0] me dara [4,1]
    Pero esto es porque le estoy cambiando la diferencia de salto, si le digo [10..0] me dara [] ya que el salto default es +1, si quiero que sea
    de 10 a 0 debo decir [10,9..0]
-}

{-
    Tipos definidos por el usuario se declaran por medio de la palabra reservada "data" y el nombre se da con mayuscula
-}

-- Lo de deriving show, significa que hereda de show las funciones
data Boleano = Falso | Verdadero deriving Show

-- Tengo una variable que es de tipo punto, pero no esta especificado,  
data Punto a = Pt a a deriving Show

-- Árboles Binarios
--Tengo una parte para pnder arboles cono nodos, y otro que pone arboles vacios.
    -- La e de en medio es la raiz del arbol
data AB e = NAB (AB e) e (AB e) | ABV deriving Show
--Puedo crear arboles de la sig forma "arbol = NAB (NAB ABV 2 ABV) 1 (NAB (NAB ABV 4 ABV) 3 (NAB ABV 5 ABV))"

arbol = NAB (NAB ABV 2 ABV) 1 (NAB (NAB ABV 4 ABV) 3 (NAB ABV 5 ABV))
-- Funcion que regresa el arbol izquierdo de un arbol binario
-- Le mando un arbol de tipo variable
izqAb :: (AB e) -> (AB e)
-- Le paso un nodo
izqAb (NAB i _ _) = i

derAb :: (AB e) -> (AB e)
-- Le paso un nodo
derAb (NAB _ _ d) = d

-- Para contar los nodos de un AB
cuentaNodosAb :: (AB e) -> Integer
cuentaNodosAb ABV = 0 --Los nodos del arbol vacio
--Le pase un nodo, y me importa ver que pasa con cada subarbol se los paso tambien como parametros
cuentaNodosAb (NAB sizq _ sder) = 1 + cuentaNodosAb sizq + cuentaNodosAb sder

-- Listas Anidadas
-- Dentro de la lista anidada tengo elementos que son lista y otros que no lo son.
-- Puedo tener elementos a y listas 
{-
data ListaAnidada a = Elemento a | Lista [ListaAnidada a] deriving Show
-- Me permite tener cosas como
    -- E 1 --Elemento 1
    -- L [E 1, E2] -- Lo que es la lista [1,2]
    -- L [E 1, L [E 1, E 2]] -- Que me genera la lista [1[1,2]]

carListaAnidada :: (ListaAnidada e) -> (ListaAnidada e)
carListaAnidada (ListaAnidada (p:_)) = p --Ya que solo me importa el primer elemento

cdrListaAnidada :: (ListaAnidada e) -> (ListaAnidada e)
cdrListaAnidada (ListaAnidada (_:xs)) = ListaAnidada xs -- Requiero decirle que es una lista anidad, ya que si no lo hago r es una lista normal
-}

{-
    Funciones lambda y de orden superior

    Las funciones lambda funcionan igual que en scheme
    Se escriben como \x -> x * x
        Donde \ es lambda
        Y -> es el . que se usa.

    La primera x que esta sola es el patron de empatamiento
    Las funciones lambda se pueden generar y usar en la ventana de dialogo
        Ejem: 
                Los tres ejemplos hacen lo mismo solo que:
                    El primero usa las operaciones de first y second para tenerlo elementos de la tupla
                    El segundo sabe que recibe una tupla y toma de ahi los valores
                    El tercero por otro lado empata con dos valores como parametro
            (\x -> fst x * snd x) (2,3)
            (\(x,y)->x*y) (2,3)
            (\x y -> x * y) 2 3

multpar = \(x,y) -> x*y

    -----Preguntar acerca de la currificacion
            Sobra decir que todas las funciones lambda estan currificadas.

    Las funciones de orden superior reciben una funcion de argumento, al igual que las funciones que reciben mas de un argumento y/o generan funciones

zip le pasas un par de listas y crea pares correspondientes

    map
        Solo trabaja en una lista, ya que las funciones solo tienen un argumento
-}

prodPuntoRecursivo :: [Integer] -> [Integer] -> Integer
prodPuntoRecursivo (p1:r1) (p2:r2) = p1*p2 + prodPuntoRecursivo r1 r2
prodPuntoRecursivo _ _ = 0  

-- Map trabaja solo con una lista y yo voy a tener dos como entrada
prodPuntoConMap:: [Integer] -> [Integer] ->Integer
prodPuntoConMap l1 l2 = sum (map (\(p,s)-> p*s)(zip l1 l2))

{-
    zip crea los pares que seran de la nueva lista en forma de pares
        tambien existe zip3

    Ejemplo con  la lista [1,2,3] [4,5,6]
    zip crea [(1,4),(2,5),(3,6)]
    Esta nueva lista entra al map
        EL map aplicara el patron a cada elemento de la lista y hara la multiplicacion
    Por ultimo genero la suma de todos los elementos de la lista nueva
zip3 = map (\((p,s),t) -> (p,s,t)) (zip (zip l1 l2)l3)
-}  

{-
    foldl y floldl1
    Van de izq a derecha son fold left
        miReversa = foldl (flip (:)) []
    Aplica el cons a los argumentos pero invertidos, empezando con una lista vacia
    Fold recibe tres argumentos lo que va a hacer, val inicial y a quien se lo hara
    Aqui no ponemos la lista a la que lo haremos para que nos regrese una funcion
    que aplicar a la lista que le demos cuando sea usado.
-}

{-
    foldr y foldr1
    Son iguales al foldl pero van de derecha a izquierda
-}

{-
    Compose (.) 
    Toma dos funciones y las une, deben ser funciones lo que uses para la compsicion
    (f.g.h)x es equivalente a f(g(h x))
    Ejem
        (sqrt.sum) [1,2,3,4,5,6]

    Puedes usar notacion infija usando ` (tecla al lado del 1 y sobre el tab)
    para que funciones
            prodPuntoRecursivo [1,2] [3,4]
        pasa a ser
            [1,2] `prodPuntoRecursivo [3,4]

    O por otro lado
        3 + 4
        (+) 3 4
-}

{-
    Haskell tiene parametros por nombre.

    Guardias
        Son estructuras condicionales.
        Ayudan a estructurar el codigo.
        Se usan cuando los patrones no son suficientemente discriminativos
-}

factGuardias :: Int -> Int
factGuardias n --Empata con un Int
                | n == 0 = 1 -- Si el entero de entrada es 0 regreso 1
                | n > 0 = n * factGuardias(n - 1) -- Si no regreso el factorial


getEvenGuardias :: [Int] -> [Int]
getEvenGuardias [] = []
getEvenGuardias (x:xs) 
                        | even x = x :getEvenGuardias xs
                        | otherwise = getEvenGuardias xs
                            -- Otherwise es como decir true

{-
    Declaracion de conjuntos
    Se puede dar en intencion o extension
        Intencion describes funcionalmente sin decir los elementos
            {x|x elemento de numeros naturales, x>0, x<10,x es impar}
        Extension digo los elementos
            {1,3,5,7,9}

    Para hacer esto usaremos comprension de lista en haskell para describir
    listas por medio de intencion. Donde establecemos que tipo de 
    valores se contendra

    [f x | x <- xs]

    La funcion f es la que usamos para los numeros
    xs es el generador de valores
-}

-- Producto cartesiano usando compension de listas
prodCartCompresionLIstas :: [a] -> [b] -> [(a,b)]
prodCartCompresionLIstas xs ys = 
                                [(x,y) | x<-xs, y<-ys]
{-
    Lo puedes leer como: 
        los pares tales que x pertenece al conjunto xs y 
        'y' pertence al conjunto ys.
    
    El simbolo '<-' puedes interpretar esa flecha como pertenece 
    que es el simbolo '∈' 
-}


prodCartCompresionLIstas2 :: (Num x) => [x] -> [y] -> [x] -> [(x,y)]
prodCartCompresionLIstas2 xs ys zs =
                                -- Puedo ahcer calculos aqui mismo 
                                [(x*z,y) | x <- xs, y <- ys, z <- zs]

positivosCompresion :: [Int] -> [Int]
-- Aqui mostramos que se pueden usar condicionlaes dentro de la compresion
positivosCompresion l = [x | x <- l, x > 0]


{-
    El where es una definicion local, por lo que no lo podemos usar fuera
    de la funcion, ya que la funcion es la que los necesita, y a pesar
    de que son tecnicamente funciones en este caso no necesitan params. 
-}
qsort :: [Int] -> [Int]
qsort [] = []
qsort (x:xs) =
            qsort lt ++ [x] ++ qsort ge
            where
                lt = [y | y <- xs, y < x]
                ge = [y | y <- xs, y >= x]


{-
    Estructuras infinitas
    [1..] esta lista genera un conjunto con los valores de 1 a infinito.
    Son los numeros naturales.

    Podemos usar take para tomar un cierto numero de elementos
    take 6
    
    Tambien existe el takeWhile que tomara elementos mientras se cumpla
    una condicional.

    takeWhile (< 30) lista
    Tomara valores de la lista que le pases que cumplan con la condicion.


    Los valores se calculan cuando se necesita. Por ello hay que tener
    cuidado cuando se usen.

    Si no pones el caso base los programas se van a ciclar.
-}