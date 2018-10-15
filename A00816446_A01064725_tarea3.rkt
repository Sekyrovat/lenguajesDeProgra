#lang scheme
; A01064725 Sergio López Madriz
; A00816446 Hector Hernandez Morales

; Funcion que regresa el menor de dos numeros
(define menor
  (lambda (x y)
    (if (< x y)
      x
      y
    )
  )
)
; Funcion que regresa el menor de 4 numeros
(define menor-4
  (lambda (n1 n2 n3 n4)
    (menor (menor (menor n1 n2) n3) n4)
  )
)
; Funcion que regresa el menor de 5 numeros
(define menor-5
  (lambda (n1 n2 n3 n4 n5)
    (menor (menor (menor (menor n1 n2) n3) n4) n5)
  )
)
#|
   Funcion que se encarga de encontrar los dos numeros menores de los 5 recibidos en la
   entrada.
|#
(define par-menor
  (lambda (n1 n2 n3 n4 n5)
    (cond
      ((= n1 (menor-5 n1 n2 n3 n4 n5))
        (display n1)
        (display '-)
        (display (menor-4 n2 n3 n4 n5))
      )
      ((= n2 (menor-5 n1 n2 n3 n4 n5))
        (display n2)
        (display '-)
        (display (menor-4 n1 n3 n4 n5))
      )
      ((= n3 (menor-5 n1 n2 n3 n4 n5))
        (display n3)
        (display '-)
        (display (menor-4 n1 n2 n4 n5))
      )
      ((= n4 (menor-5 n1 n2 n3 n4 n5))
        (display n4)
        (display '-)
        (display (menor-4 n1 n2 n3 n5))
      )
      ((= n5 (menor-5 n1 n2 n3 n4 n5))
        (display n5)
        (display '-)
        (display (menor-4 n1 n2 n3 n4))
      )
    )
  )
)

#|
   Funcion que se encarga de calcular de forma recursiva el logaritmo de un numero
   en el intervalo (-1, 1], utlizando la formula sumatoria i=1..n (-1)^(i-1)*x^i/i
   siendo n el segundo parametro de la funcion.
|#
(define logaritmo
  (lambda (x n (accum 0))
    (if (zero? n)
      accum
      (logaritmo
        x
        (- n 1) 
        (+ accum (* (expt -1 (- n 1)) (/ (expt x n) n)))
      )
    )
  )
)

; Funcion encargada de mostrar la secuencia en orden ascendente
(define aux-secuencia-ascendente
  (lambda (n)
    (if (zero? n)
      (void)
      (void
        (aux-secuencia-ascendente (- n 1))
        (display n) (display " ")
      )
    )
  )
)
; Funcion encargada de mostrar la secuencia en orden descendente
(define aux-secuencia-descendente
  (lambda (n)
    (if (zero? n)
      (newline)
      (void
        (display n)(display " ")
        (aux-secuencia-descendente (- n 1))
      )
    )
  )
)
#|
   Funcion para desplegar n secuencias de numeros del 0 a m.
   Se apoya de funciones auxiliares, para desplegar.
   Se verifica el valro de accum para saber que orden debe tener la secuencia
   Cada vez que se termina de desplegar se llama a una nueva secuencia con accum + 1
|#
(define secuencias
  (lambda (n m (accum 0))
    (cond
      ((= n accum)
        void
      )
      ((even? accum)
        (void
          (aux-secuencia-ascendente m)
          (newline)
          (secuencias n m (+ accum 1))
        )
      )
      (else
        (void
          (aux-secuencia-descendente m)
          (secuencias n m (+ accum 1))
        )
      )
    )
  )
)

; Funcion que genera las repeticiones del numero leido
(define aux-repite
  (lambda (num (listaSalida '()) (count 0))
    (if (= num count)
      listaSalida
      (aux-repite num (cons num listaSalida) (+ count 1))
    )
  )
)
#|
   Funcion que recibe una lista con la que se trabajara obteniendo los valores
   enteros incluidos en ella, y pasara cada entero a la funcion auxiliar
   para que dicha funcion obtenga una lista con las repeticiones
|#
(define repite
  (lambda (listaEntrada (listaSalida '()))
    (if (null? listaEntrada)
      (reverse listaSalida)
      (repite
        (cdr listaEntrada)
        (aux-repite (car listaEntrada) listaSalida)
      )
    )
  )
)

; Funcion que funge como auxilair de contadores
(define aux-contadores
  (lambda (listaEntrada numero (listaSalida '()) (accum 0))
    (cond
      ; Se han contado todos los numeros, entregamos el resultado
      ((zero? numero)
        listaSalida
      )
      #|
         La lista se acabo por lo que nos quedamos en ella, imprimimos el valor de accum
         para el numero actual y todos los demas accum se imrpimira con 0.
      |#
      ((null? listaEntrada)
        (aux-contadores
          listaEntrada
          (- numero 1)
          (cons accum listaSalida)
        )
      )
      #|
         En caso de que el numero concuerde con el valor de la lista en que estamos
         posicionados aumentamos el valor del accum en 1.
      |#
      ((= numero (car listaEntrada))
        (aux-contadores
          (cdr listaEntrada)
          numero
          listaSalida
          (+ accum 1)
        )
      )
      #|
         Cuando el numero deja de concordar generamos la celda cons guardando
         el valor de accum, y reducimos el valor de numero.
      |#
      (else
        (aux-contadores
          listaEntrada
          (- numero 1)
          (cons accum listaSalida)
        )
      )
    )
  )
)
#|
   Funcion que recibe la lista en orden descendente, y llama a aux-contadores
   mandandole como parametros la lista y el valor maximo de esta
|#
(define primer-contadores
  (lambda (lista)
    (aux-contadores lista (car lista))
  )
)
#|
   Funcion primaria que recibe la lista, la ordena de mayor a menor y la usa
   como parametro en la funcion auxiliar primer-contadores
|#
(define contadores
  (lambda (lista)
    (primer-contadores (sort lista >))
  )
)

; Funcion auxiliar de enteros
(define aux-enteros
  (lambda (lista (accum 0))
    (cond
      #|
         Verificamos si la lista es nula, en cuyo caso se o se
         termino la lista de parametros o una de las sublistas
      |#      
      ((null? lista)
        accum
      )
      #|
         En este caso se leyo un entero, por lo que le agregamos uno al accum
      |#
      ((exact-integer? (car lista))
        (aux-enteros (cdr lista) (+ accum 1))
      )
      #|
         Se leyo una celda, por lo que sabemos que estamos en la lista de listas
         y debemos procesar la lista que se leyo.
      |#
      ((pair? (car lista))
        (aux-enteros (cdr lista) (+ accum (aux-enteros (car lista))))
      )
      ; Se leyo un no-numero
      (else
        (aux-enteros (cdr lista) accum)
      )
    )
  )
)
#|
   Funcion que tiene la posibilidad de recibir una cantidad variable de parametros,
   debido a que listas no esta entre parentesis. Se llama a la funcion auxiliar
   encargada de hacer el procedimiento.
|#
(define enteros
  (lambda listas
    (aux-enteros listas)
  )
)

; Funcion auxiliar para la funcion forma
(define forma-row
  (lambda (lista m (row '()))
    (cond
      #|
         En caso de que m sea cero se termino de generar la secuencia, se le hace un reverse
         y se regresa.
      |#
      ((zero? m)
        (reverse row)
      )
      ; Si la lista se acabo antes de terminar, llenamos el espacio con -
      ((null? lista)
        (forma-row lista (- m 1) (cons '- row))
      )
      ; Si se leyo un caracter se genera la celda cons con el valor leido. 
      (else
        (forma-row (cdr lista) (- m 1) (cons (car lista) row))
      )
    )
  )
)
; Funcion que recorre la lista de valores la cantidad de veces que es necesario.
(define cdnr
  (lambda (lista n)
    (if (or (null? lista) (zero? n))
      lista
      (cdnr (cdr lista) (- n 1))
    )
  )
)

#|
  Funcion que regresara una lista con n sublistas de m elementos,
  con apoyo en caso de no completar.
|#
(define forma
  (lambda (lista n m (salida '()))
    ; Primero verificamos si n es 0, en cuyo caso se termino de generar las secuencias
    (if (zero? n)
      ; Desplegamos la lista de secuencias en orden inverso.
      (reverse salida)
      #|
         Si no es 0 llamamos a la funcion que recorre la lista de valores para posicionarnos
         en el siguiente valor que debe poner en una secuencia; ademas de generar la lista
         que tendra la secuencia. Esto se hace con cdnr y forma-row respectivamente
      |#
      (forma (cdnr lista m) (- n 1) m (cons (forma-row lista m) salida))
    )
  )
)

#|
   Función que dada una lista propia posiblemente imbricada, regresa una lista de la misma
   forma con numeros en cada posición indicando profundidad.posición, limite de 9 elementos
   por nivel de la lista.
|#
(define enumera
  (lambda (listaEntrada (profundidadActual 0) (listaSalida '()) (elementoActual 0.1))
    (cond
      ((null? listaEntrada)
        (reverse listaSalida)
      )
      ((pair? (car listaEntrada))
        (enumera
          (cdr listaEntrada)
          profundidadActual
          (cons (enumera (car listaEntrada) (+ profundidadActual 1)) listaSalida)
          (+ elementoActual 0.1)
        )
      )
      (else
        (enumera
          (cdr listaEntrada)
          profundidadActual
          (cons (+ profundidadActual elementoActual 1) listaSalida)
          (+ elementoActual 0.1)
        )
      )
    )
  )
)