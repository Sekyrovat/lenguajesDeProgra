#lang scheme
; A00816446
; A01064725

; Funcion auxiliar de la funcion aplica-listas
(define aplica-procedimiento
  #|
     Recibe el procedimiento a aplicar mas las listas a las que debe aplicar el
     procedimiento mas la lista en la que concatenara las listas de resultados
  |#
  (lambda (procedimiento lista1 lista2 (salida '()))
    ; Vemos dependiendo si una de las listas de entrada se a acabado, en cuyo caso regresamos
    (if (or (null? lista1) (null? lista2))
      (reverse salida)
      ; En esta parte aplicamos el procedimiento al siguiente elemento de la lista
      (aplica-procedimiento
        procedimiento
        (cdr lista1)
        (cdr lista2)
        ; Genramos el elemento resultante de la aplicacion
        (cons
          (procedimiento
            (car lista1)
            (car lista2)
          )
          salida
        )
      )
    )
  )
)

; Funcion que recibe una serie de argumentos mas las listas a las que debe aplicarlos
(define aplica-listas
  (lambda (procedimientos lista1 lista2 (salida '()))
    ; Mientras la lista de procedimientos no acabe seguimos llamando a la funcion auxiliar
    (if (null? procedimientos)
      ; Al acabar regresamos la lista de salida con las listas evaluadas
      (reverse salida)
      (aplica-listas
        (cdr procedimientos)
        lista1
        lista2
        (cons
          (aplica-procedimiento
            (car procedimientos)
            lista1
            lista2
          )
          salida
        )
      )
    )
  )
)

; Funcion que verifica si se cumple o no con el predicado basado en las listas entrantes
(define alguno?
  (lambda (predicado lista1 lista2)
    ; Si una lista se termina sin haber cumplido con el predicado entonces desplegamos #f
    (if (or (null? lista1) (null? lista2))
      #f
      #|
         Por medio de un or realizamos la secuencia de predicados con el argumento
         correspondiente, ya que el or trunca en el primer true
      |#
      (or
       (predicado (car lista1) (car lista2))
       ; Continuamos evaluando el resto de las listas
       (alguno? predicado (cdr lista1) (cdr lista2))
      )
    )
  )
)

; Funcion que muestra el resultado de forma explicita de la funcion que se defina
(define verbosa
  ; Se currifica la entrada para poder procesar tanto la funcion como argumentos.
  (lambda (x)
    (lambda (y)
      (display "Entrada = ")
      (display y)
      (newline)
      (display "Salida = ")
      (display (x y))
      (newline)
      (x y)
    )
  )
)