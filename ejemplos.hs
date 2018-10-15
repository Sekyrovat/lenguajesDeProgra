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

