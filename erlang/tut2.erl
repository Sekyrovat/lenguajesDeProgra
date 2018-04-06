%Los modulos tienen las funciones
-module(tut2).

% Importamos funciones
				%Importe lenght y le doy el numero de atributos '
				% que recibe
-import(string, [len/1, concat/2, chr/2, substr/3, str/2, 
	to_lower/1, to_upper/1]).

% Export son las funciones que podemos exportar del modulo
-export([main/0]).

% Podemos llamar funciones antes de que sean definidas.
main() ->
	var_stuff(),
	atom_stuff(),
	do_math(5 ,4),
	compare(4, 4.0),
	ejemploComparacion(),
	what_grade(10),
	say_hello(german),
	string_stuff(),
	list_stuff(),
	% List comprehension
	lc_stuff(),
	type_stuff(),
	find_factorial(3).

% Nombres de variables empiezan con mayuscula.
% Los tipos se dan de forma dinamica,
% pero no podemos cambiar el valor.
var_stuff()->
	% Definimos una variable, y regresamos su valor.
	Num = 1,
	Num.

% Un atomo su nombre es igual a su valor. 
% Van con letra minuscula
% Imprime "an_atom" sin las comillas
atom_stuff()->
	an_atom.

% Recibe dos atributos
do_math(A,B) ->
	A + B,
	A - B,
	A * B,
	A / B,
	A div B,
	A rem B,
	% Lo siguiente nos da e^1
	math:exp(1),
	math:log(math:exp(1)),
	math:log10(1000),
	% a^b
	math:pow(A,B),
	math:sqrt(A),
	%Genera un numero random entre 1 y A
	rand:uniform(A).

compare(A, B) ->
	%Checamos por valor y tipo
	A =:= B, % 4 y 4.0 no son de igual tipo
	A =/= B, % No iguales en valor y tipo
	A == B, % No presta atencion al tipo
	A /= B,
	A  > B,
	A < B,
	A >= B,
	A =< B.

ejemploComparacion() ->
	Age = 18,
	(Age >= 5) or (Age =< 18).
	% Regresa true ya que Age vale 18

preschool() ->
	'Go to preschool'.
kindergarten() ->
	'Go to kindergarten'.
grade_school() ->
	'Go to grade school'.

what_grade(X) ->
	if
		X < 5 -> preschool();
		X == 5 -> kindergarten();
		X > 5 -> grade_school()
	end.

say_hello(X) ->
	case X of
		french -> 'Bonjour';
		german -> 'Guten tag';
		english -> 'Hi'
	end.

string_stuff() ->
	Str1 = "Random string",
	Str2 = "A new random",

	% Escribimos siguiendo el patron que se de
	io:fwrite("String : ~p ~p\n", [Str1, Str2]),

	Str3 = io_lib:format("It's a ~s and ~s\n", [Str1, Str2]),
	io:fwrite(Str3),

	% Obttenemos la longitud del string
	len(Str3),

	Str4 = concat(Str1, Str2),
	Str4,

	% Obtenemos el index de un char en particular.
	CharIndex = chr(Str4, $n),
					% Ponemos el string en que buscamos
					% y el char que buscamos depues de '$'
	CharIndex,

	% Definimos donde empezamos y el numero que deseamos recorrer
	Str5 = substr(Str4, 8, 6),
	Str5,

	% Con esto obtenemos el index del string que buscamos.
		% en este caso buscamso str2 en str4.
	StringIndex = str(Str4, Str2),
	StringIndex,

	CapitalStr = to_upper(Str1),
	LowerStr = to_lower(Str1),

	CapitalStr,
	LowerStr.

list_stuff() ->
	List1 = [1,2,3],
	List2 = [4,5,6],

	% Append
	List3 = List1 ++ List2,
	List3,

	% Elimina de la Lista3 los elementos de la Lista 1
	List4 = List3 -- List1,
	List4,

	% Head
	hd(List4),
	% Tail
	tl(List4),

	List5 = [3 | List4],
	List5,

	[Head|Tail] = List5,
	Tail.

lc_stuff() ->
	List1 = [1,2,3],
	% Queremos multiplicar cada elemento de la lista por 2.
	% La lista 2 es igual a:
		% Operacion que deseamos realizar.
		% Dos pipes
		% Definimos el nombre temporal de cada elemento 
			% que tomamos de la lista
	List2 = [2*N || N <- List1],
	List2,

	List3 = [1,2,3,4],
		% Aqui le damos al condicion con la que deben cumplir.
			% La condicion aqui es que sean pares.
	List4 = [N || N <- List3, N rem 2 == 0],
	List4,

	City_Weather = [{pittsburgh, 50}, {chicago, 20}, 
					{'New York', 30}],

	% Queremos encontrar las ciudades con mayor temp que 20
	Great_Temp = [{City, Temp} || {City, Temp} <- City_Weather, Temp > 20],
	Great_Temp.

type_stuff() ->
	% Vemos si lo que hay en el parentesis es lo que corresponde.
	is_atom(name),
	is_float(3.4),
	is_integer(10),
	is_boolean(false),
	is_list([1,2,3]),
	is_tuple({height, 6.24}),

	% Puedes convertir de uno a otro como type_to_type

	List1 = integer_to_list(21),
	List1.

factorial(N) when N == 0 -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

find_factorial(X) ->
	Y = factorial(X),
	io:fwrite("Factorial: ~p\n", [Y]).

