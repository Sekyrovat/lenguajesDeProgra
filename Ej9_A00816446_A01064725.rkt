#lang scheme
; A01064725
; A00816446

; Funcion para regresar el numero de 1os en una lista dada
(define (unos lista (accum 0))
  (cond
    ((null? lista)
      accum
    )
    ((= 1 (car lista))
      (unos (cdr lista) (+ 1 accum))
    )
    (else
      (unos (cdr lista) accum)
    )
  )
)

; Funcion de utilidad para invertir los numeros.
(define (inversion-binaria numero)
  (if (= 1 numero)
    0
    1
  )
)
#|
   Funcion con la intencion de tomar los elementos del renglon y pasarlos a la funcion
   inversion-binaria.
   Reconstruye el renglon de forma inversa y lo regresa a su forma original antes de
   regresalo a la funcion principal.
|#
(define (invierte-renglon renglonEntrada (renglonSalida '()))
  (if (null? renglonEntrada)
    (reverse renglonSalida)
    (invierte-renglon
      (cdr renglonEntrada)
      (cons (inversion-binaria (car renglonEntrada)) renglonSalida)
    )
  )
)
#|
   Funcion invierte, que dada una matriz le hara el complemento binario
   a los valores binarios presentes
|#
(define (invierte matrizEntrada (matrizSalida '()))
  (if (null? matrizEntrada)
    (reverse matrizSalida)
    (invierte
      (cdr matrizEntrada)
      (cons (invierte-renglon (car matrizEntrada)) matrizSalida)
    )
  )
)

; Funcion de utiliad para obtener la califiacion del parcial dado si existe, si no regresa NO
(define (obten-parcial numeroParcial listaCalificaciones)
  (cond
    ((null? listaCalificaciones)
      'NO
    )
    ((= numeroParcial 1)
      (car listaCalificaciones)
    )
    (else
      (obten-parcial (- numeroParcial 1) (cdr listaCalificaciones))
    )
  )
)
; Forma la lista que contiene la matricula y el resultado del parcial especificado
(define (matricula-calificacion numeroParcial entrada)
  (cons
    (car entrada)
    (cons
      (obten-parcial numeroParcial (caddr entrada))
      '()
    )
  )
)
#|
   Funcion parcial que dada una tabla de datos regresa el parcial especificado del alumno
   en caso de que exista, de lo contrario, regresa NO.
|#
(define (parcial numeroParcial tabla (listaSalida '()))
  (if (null? tabla)
    (reverse listaSalida)
    (parcial
      numeroParcial
      (cdr tabla)
      (cons
        (matricula-calificacion numeroParcial (car tabla))
        listaSalida
      )
    )
  )
)

#|
   Funcion para encontrar la posicion y el subarbol correspondiente al nodo que se
   indica en caso de existir.
|#
(define (aux-subarbol identificadorNodo arbol)
  (cond
    ((null? arbol)
      #f
    )
    ((eqv? identificadorNodo (car arbol))
      arbol
    )
    (else
      (or
        (aux-subarbol identificadorNodo (cadr arbol))
        (aux-subarbol identificadorNodo (caddr arbol))
      )
    )
  )
)
; Funcion de subarbol que regresa el subarbol generado a partir del nodo especificado.
(define (subarbol identificadorNodo arbol)
  (or (aux-subarbol identificadorNodo arbol) '())
)

#|
   Funcion auxiliar para obtener el nodo adyacente si es que existe y
   regresa falso en caso de que no exista.
|#
(define (nodo-adyacente-arista nodo arista)
  (cond
    ((eqv? nodo (car arista))
      (cadr arista)
    )
    ((eqv? nodo (cadr arista))
      (car arista)
    )
    (else
      #f
    )
  )
)

#|
   Funcion auxiliar para la funcion inicial, en la que llamaremos a la funcion de
   nodo-adyacente-arista con cada par que se de, para encontrar los nodos que
   cumplan con el requerimiento dado.
|#
(define (aux-adyacentes nodo listaAristas (listaAdyacentes '()))
  (cond
    ((null? listaAristas)
      (reverse (remove-duplicates listaAdyacentes))
    )
    ((nodo-adyacente-arista nodo (car listaAristas))
      (aux-adyacentes
        nodo
        (cdr listaAristas)
        (cons
          (nodo-adyacente-arista nodo (car listaAristas))
          listaAdyacentes
        )
      )
    )
    (else
      (aux-adyacentes nodo (cdr listaAristas) listaAdyacentes)
    )
  )
)

; Funcion principal para obtener los nodos adyacentes
(define (adyacentes nodo grafo)
  (aux-adyacentes nodo (cadr grafo))
)