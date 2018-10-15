# Implementacion de un scanner mediante la codificacion de un Automata
# Finito Determinista como una Matriz de Transiciones
# Autor: Dr. Santiago Conant, Agosto 2014 (modificado en Agosto 2015)
#!/usr/bin/python3
import sys

# tokens
INT = 100  # Numero entero
FLT = 101  # Numero de punto flotante
OPB = 102  # Operador binario
LRP = 103  # Delimitador: parentesis izquierdo
RRP = 104  # Delimitador: parentesis derecho
END = 105  # Fin de la entrada
ERR = 200  # Error lexico: palabra desconocida

# Matriz de transiciones: codificacion del AFD
# [renglon, columna] = [estado no final, transicion]
# Estados > 99 son finales (ACEPTORES)
# Caso especial: Estado 200 = ERROR
#      dig   op   (    )  raro  esp  .   $
MT = [[  1, OPB, LRP, RRP,   4,   0, 4, END], # edo 0 - estado inicial
      [  1, INT, INT, INT, INT, INT, 2, INT], # edo 1 - digitos enteros
      [  3, ERR, ERR, ERR,   4, ERR, 4, ERR], # edo 2 - primer decimal flotante
      [  3, FLT, FLT, FLT, FLT, FLT, 4, FLT], # edo 3 - decimales restantes flotante
      [ERR, ERR, ERR, ERR,   4, ERR, 4, ERR]] # edo 4 - estado de error

# Filtro de caracteres: regresa el numero de columna de la matriz de transiciones
# de acuerdo al caracter dado
def filtro(c):
    """Regresa el numero de columna asociado al tipo de caracter dado(c)"""
    if c == '0' or c == '1' or c == '2' or \
       c == '3' or c == '4' or c == '5' or \
       c == '6' or c == '7' or c == '8' or c == '9': # digitos
        return 0
    elif c == '+' or c == '-' or c == '*' or \
         c == '/': # operadores
        return 1
    elif c == '(': # delimitador (
        return 2
    elif c == ')': # delimitador )
        return 3
    elif c == ' ' or ord(c) == 9 or ord(c) == 10 or ord(c) == 13: # blancos
        return 5
    elif c == '.': # punto
        return 6
    elif c == '$': # fin de entrada
        return 7
    else: # caracter raro
        return 4

# Funcion principal: implementa el analisis lexico
def scanner():
    """Implementa un analizador lexico: lee los caracteres de la entrada estandar"""
    edo = 0 # numero de estado en el automata
    lexema = "" # palabra que genera el token
    tokens = []
    leer = True # indica si se requiere leer un caracter de la entrada estandar
    while (True):
        while edo < 100:    # mientras el estado no sea ACEPTOR ni ERROR
            if leer: c = sys.stdin.read(1)
            else: leer = True
            edo = MT[edo][filtro(c)]
            if edo < 100 and edo != 0: lexema += c
        if edo == INT:    
            leer = False # ya se leyo el siguiente caracter
            print("Entero", lexema)
        elif edo == FLT:   
            leer = False # ya se leyo el siguiente caracter
            print("Flotante", lexema)
        elif edo == OPB:   
            lexema += c  # el ultimo caracter forma el lexema
            print("Operador", lexema)
        elif edo == LRP:   
            lexema += c  # el ultimo caracter forma el lexema
            print("Delimitador", lexema)
        elif edo == RRP:  
            lexema += c  # el ultimo caracter forma el lexema
            print("Delimitador", lexema)
        elif edo == ERR:   
            leer = False # el ultimo caracter no es raro
            print("ERROR! palabra ilegal", lexema)
        tokens.append(edo)
        if edo == END: return tokens
        lexema = ""
        edo = 0
            
        
    

