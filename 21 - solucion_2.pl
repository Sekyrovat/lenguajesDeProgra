%% SOLUCIÓN DEL EJERCICIO DE CLASE 21

% (2) implementa el bicondicional (<=>)
bicond(A, B) :- A, B, !.
bicond(A, B) :- \+A, \+B.
% ?- bicond(true,true). 	=> true.
% ?- bicond(true,false).	=> false.
% ?- bicond(2 < 3,7 > 2). 	=> true.
% ?- bicond(2 > 3,7 > 2).	=> false.

% (3) cuente los nodos internos de un árbol binario (que tiene al menos 
%     un subárbol no vacío) descrito con la función: 
%     arbol(Raíz, SubárbolIzquierdo, SubárbolDerecho).
internos(arbol(_, nil, nil), 0) :- !.
internos(arbol(_, I, D), N) :- !,
   internos(I, Iizq),
   internos(D, Ider),
   N is Iizq + Ider + 1.
internos(nil, 0).
% ?- internos(arbol(8,arbol(5,arbol(2,nil,nil),arbol(7,nil,nil)),
%    arbol(9, nil, arbol(15, arbol(11, nil, nil), nil))),I).  => I = 4.

% (4) busque un elemento, que debe pedirle al usuario, en un árbol 
%     binario y que si lo encuentra, le reste una cantidad solicitada; 
%     de lo contrario que regrese el árbol original.
decrementa(nil, nil) :- !.
decrementa(Arbol, NArbol) :- 
   write("Elemento? "),
   read(E),
   encuentra(E, Arbol), !,
   write("Decremento? "),
   read(D),
   reduce(E, D, Arbol, NArbol).
decrementa(Arbol, Arbol) :-
   write("No se encuentra el arbol"), nl.
   
% verifica si un valor es elemento de un árbol
encuentra(E, arbol(E, _, _)) :- !.
encuentra(E, arbol(_, I, _)) :- encuentra(E, I), !.
encuentra(E, arbol(_, _, D)) :- encuentra(E, D).

% reduce el valor de un elemento existente de un árbol
reduce(E, R, arbol(E, I, D), arbol(NE, NI, ND)) :- !,
   NE is E - R,
   reduce(E, R, I, NI),
   reduce(E, R, D, ND).
reduce(E, R, arbol(V, I, D), arbol(V, NI, ND)) :- !,
   reduce(E, R, I, NI),
   reduce(E, R, D, ND).
reduce(_, _, nil, nil).
% ?- decrementa(arbol(8,arbol(5,arbol(10,nil,nil),arbol(7,nil,nil)),
%    arbol(9, nil, arbol(15, arbol(11, nil, nil), nil))), A).  
% Elemento? 10.
% Decremento? 4.
% => A = arbol(8,arbol(5,arbol(6,nil,nil),arbol(7,nil,nil)),
%                 arbol(9, nil, arbol(15, arbol(11, nil, nil), nil)))
% ?- decrementa(arbol(8,arbol(5,arbol(2,nil,nil),arbol(7,nil,nil)), 
%                       nil), A).  
% Elemento? 1
% No se encuentra en el árbol
% => A = arbol(8,arbol(5,arbol(2,nil,nil),arbol(7,nil,nil)), nil).

