%% SOLUCIÓN AL EJERCICIO DE CLASE # 19

% Pedro padece gripe.
padece(pedro, gripe).
% Pedro padece hepatitis.
padece(pedro, hepatitis).
% Juan padece hepatitis.
padece(juan, hepatitis).
% María padece gripe.
padece(maria, gripe).
% Carlos padece intoxicación.
padece(carlos, intoxicacion).
% La fiebre es sintoma de gripe.
sintoma(fiebre, gripe).
% El cansancio es síntoma de hepatitis.
sintoma(cansancio, hepatitis).
% La diarrea es síntoma de intoxicación.
sintoma(diarrea, intoxicacion).
% La aspirina suprime la fiebre.
suprime(aspirina, fiebre).
% El lomotil suprime la diarrea.
suprime(lomotil, diarrea).
% Un fármaco alivia una enfermedad si la enfermedad tiene un síntoma
% que sea suprimido por el fármaco
alivia(Farmaco, Enfermedad) :-
   sintoma(Sintoma, Enfermedad),
   suprime(Farmaco, Sintoma).
% Una persona debería tomar un fármaco si padece una enfermedad que 
% sea aliviada por el fármaco.
debe_tomar(Persona, Farmaco) :-
   padece(Persona, Enfermedad),
   alivia(Farmaco, Enfermedad).
   
%% 	Queries

% a) ¿quién padece gripe?
% ?- padece(Quien, gripe).
% b) ¿qué síntomas tiene Pedro?
% ?- padece(pedro, Enfermedad), sintoma(Sintoma, Enfermedad).
% ¿quién padece diarrea?
% ?- sintoma(diarrea, Enfermedad), padece(Quien, Enfermedad).
% ¿quién esta cansado?
% ?- sintoma(cansancio, Enfermedad), padece(Quien, Enfermedad).
% ¿hay algún fármaco que alivie a Pedro?
% ?- debe_tomar(pedro, Farmaco).
% ¿ hay algún síntoma que compartan Juan y María?
% ?- padece(juan, Ejuan), sintoma(Sintoma, Ejuan), padece(maria, Emaria), sintoma(Sintoma, Emaria).