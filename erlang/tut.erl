%Los modulos tienen las funciones
-module(tut).

% Importamos funciones
				%Importe lenght y le doy el numero de atributos que recibe
-import(string, [len/1, concat/2, chr/2, substr/3, str/2, to_lower/1, to_upper/1]).

% Export son las funciones que podemos exportar del modulo
-export([hello_world/0, add/2, add/3]).

hello_world() -> 
	io:fwrite("Hello World\n").

add(A,B)->
	% Llamamos otra funcion del mismo archivo dentro de la funcion
	% pero lo separamoscon coma.
	hello_world(),
	A + B.
% Aqui se peude ver lo que se da con el polimorfismo que nos 
% permite tener dos funciones con el mismo nombre pero que 
% al recibir un diferente numero de argumentos se consideran
% diferentes y se sabe cual usar dependiendo del numeor de 
% parametros de entrada.
add(A, B, C) ->
	%hello_world(),
	A + B + C.
