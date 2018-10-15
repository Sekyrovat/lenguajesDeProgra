#lang scheme
; A00816446
; A01064725
; Función que calcula la profundidad máxima de una lista propia.
(define (profundidad-maxima lista (profundidadActual 0))
  (cond
    ((null? lista)
      profundidadActual
    )
    ((list? (car lista))
      (max
        (profundidad-maxima (car lista) (+ profundidadActual 1))
        (profundidad-maxima (cdr lista) profundidadActual)
      )
    )
    (else
      (max
        (+ profundidadActual 1)
        (profundidad-maxima (cdr lista) profundidadActual)
      )
    )
  )
)
; Función que determina si dentro de la lista dada existe algún elemento a la profundidad dada
(define (profundidad? lista profundidad)
  (>= (profundidad-maxima lista) profundidad)
)

; Función que forma renglones compuestos de el valor dado el número de columnas dado. 
(define (forma-renglones columnas valor (renglon '()) (accumColumnas 0))
  (if (= columnas accumColumnas)
    renglon
    (forma-renglones columnas valor (cons valor renglon) (+ accumColumnas 1))
  )
)
; Función que construye una tabla de reglones x columnas compuesta del valor dado.
(define (tabla renglones columnas valor (tablaSalida '()) (accumRenglones 0))
  (if (= renglones accumRenglones)
    tablaSalida
    (tabla
      renglones
      columnas
      valor
      (cons (forma-renglones columnas valor) tablaSalida)
      (+ accumRenglones 1)
    )
  )
)

; Función auxiliar para la función concatena.
(define (aux-concatena listaEntrada listaSalida)
  (cond
    ((null? listaEntrada)
      listaSalida
    )
    ((list? (car listaEntrada))
      (aux-concatena
        (cdr listaEntrada)
        (cons (reverse (aux-concatena (car listaEntrada) '())) listaSalida)
      )
    )
    (else
      (aux-concatena (cdr listaEntrada) (cons (car listaEntrada) listaSalida))
    )
  )
)
#| Función que concatena el contenido de las sublistas de una lista posiblemente
   imbricada, eliminando cualquier elemento que no sea parte de una sublista. |#
(define (concatena listaEntrada (listaSalida '()))
  (cond
    ((null? listaEntrada)
      (reverse listaSalida)
    )
    ((list? (car listaEntrada))
      (concatena
        (cdr listaEntrada)
        (aux-concatena (car listaEntrada) listaSalida)
      )
    )
    (else
      (concatena (cdr listaEntrada) listaSalida)
    )
  )
)