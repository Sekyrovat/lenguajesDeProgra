% A01064725
% A00816446

%Los modulos tienen las funciones
-module(ej1).

-import (math, [sqrt/1]).
% Export son las funciones que podemos exportar del modulo
-export([formula_general/3, secuencia/1, mapea/2, menores/2]).

% Primero vemos si el valor de A es 0
formula_general(0, _, _) ->
	% En caso de que lo sea reportamos el error e ignoramos B y C.
	io:fwrite("Error el valor de a no puede ser 0~n");
% En caso de que no hayamos empatado con el patron anterior procedemos.
formula_general(A, B, C) ->
	% Mediante un case evaluamos lo que esta dentro de la raiz de la
	% formula general, para optimizar la obtencion de resultados.
	case math:pow(B,2) - 4 * A * C of
		% Si el resultado es 0, no debemos calcular la raiz,
		% y sera el mismo valor para ambos resultados.
		0 ->
			% Solo calulamos -b/2a
			OUT = -B / (2 * A),
			{OUT, OUT};
		% Si llegamos aqui fue porque el resultado de la expresion
		% no fue 0, en cuyo caso debemos ver que sea mayor a 0.
		P1 when P1 > 0 ->
			% Almacenamos el resultado de la raiz en una variable
			% Esto para efiecientizar el proceso.
			P2 = math:sqrt(P1),
			% Realizamos los calculos para desplegar.
			{(-B + P2) / (2 * A), (-B - P2) / (2 * A)};
		% Si llegamos a este punto es porque el resultado de la
		% raiz es menor a 0, por lo que desplegamos 'imaginarias'
		_ ->
			imaginarias
	end.


%  En caso de que N sea 0 podemos regresar la lista vacia
secuencia_aux(0) -> [];
% En caso de que N sea otro valor lo agregamos a la lista y llamamos de forma recursiva con N-1
secuencia_aux(N) -> [N | secuencia_aux(N-1)].

% Ya que la auxiliar nos regresa la lista al revez debemos hacer un reverse a la lista.
secuencia(N) -> lists:reverse(secuencia_aux(N)).


% Cuando recibimos una lista vacia regresamos la lista vacia
mapea(_, []) -> [];
% Con esta funcion vamos generando la lista aplicando la funcion a cada elemento y llamando de forma
% recursiva a la funcion con el resto de la lista. 
mapea(Funcion, [Elemento | Resto]) -> [Funcion (Elemento) | mapea(Funcion, Resto)].

% FUncion que nos regresa los elementos de una lista menores a un numero dado
menores(List, N) ->
	% Mapearemos las sublistas de la lista para ser procesadas por filtermap, utilizando la condicion 
	% de que el elemento sea menor que el argumento N.
	lists:map(fun(SubList) -> lists:filtermap(fun(X) -> X < N end, SubList) end, List).