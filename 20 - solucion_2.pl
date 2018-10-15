%% SOLUCIÓN DEL EJERCICIO DE CLASE # 20

% (2) despliega la secuencia N,N-1,N-2,…,1 saltando de línea después de cada número.
secuencia(N) :- 
   N > 1, 
   write(N), nl, 
   N1 is N-1, secuencia(N1).
secuencia(1) :- write(1), nl.
%?- secuencia(3).	
% => 3
%    2
%    1

% (3) despliega los múltiplos de N menores o iguales a M (incluyendo a N) separando cada 
% múltiplo con un espacio.
multiplos(N, M) :- mult_aux(N, M, N).
mult_aux(N, M, A) :- 
   A =< M, 
   write(A), write(' '), 
   NM is A + N, 
   mult_aux(N, M, NM).
mult_aux(_, M, A) :- A > M, nl.
% ?- multiplos(2,10).	=> 2 4 6 8 10
% ?- multiplos(3,20).	=> 3 6 9 12 15 18

% (4) obtenga la cantidad de dígitos que tenga un número entero no negativo.
digitos(N, D) :- 
   N > 9, 
   NN is N // 10,
   digitos(NN, ND),
   D is ND + 1.
digitos(N, 1) :- N < 10.
% ?- digitos(7,1).		=> true
% ?- digitos(1234,D).	=> D = 4
