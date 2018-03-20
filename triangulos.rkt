#lang racket
(define (triangulo l1 l2 l3)
  (if (triang? l1 l2 l3)
      (cond ((= l1 l2 l3) 'equilatero)
            ((or (= l1 l2) (= l1 l3) (= l2 l3) 'isoceles))
             (else 'escaleno))
      'no-triangulo))


(define (triang? l1 l2 l3)
  (and (> (+ l1 l2) l3)
       (> (+ l1 l3) l2)
       (> (+ l2 l3) l1)))

;calcula el factorial de un numero
(define (factorial n)
  (if (zero? n)
      1
      (* n (factorial (- n 1)))))

;eleva un valor a una potencia no negativa
(define (a^b a b)
  (if (zero? b)
      1
      (* a (a^b a (- b 1)))))

;calculo de una serie
(define (sumat a)
  (+ (/ 2.0 a) 1.0))

(define (serie a)
  (if (= a 1)
      (sumat 1)
      (+ (serie(- a 1)) (sumat a)))
)

;Cunta y regresa cantidad de impares
(define (impares a)
  (define-values (q r) (quotient/remainder a 10))
  (if (zero? q)
    (if (odd? r)
      1
      0
    )
    (if (odd? r)
      (+ (impares q) 1)
      (impares q)
    )
  )  
)

(define (fibo3 a)
  (if (< a 4)
    1
    (+ (fibo3 (- a 1)) (fibo3 (- a 2)) (fibo3 (- a 3)))
  )
)

(define (fibo3t a)
  (if (< a 4)
    1
    (fibo3t-helper a 1 1 1)
  )
)

(define (fibo3t-helper a h1 h2 h3)
  (if (= a 3)
    h3
    (fibo3t-helper (- a 1) h2 h3 (+ h1 h2 h3))
  )
)


;De la lista vamos a generar un valor atomico
;contar los elementos de una lista
(define (cuenta lista)
  (if ( null? lista)
      0
      (+ 1 (cuenta (cdr lista))))
)


;De un atomo generamos una lista
;Generar lista con n ceros
(define (ceros n)
  ;Caso mas simple una lista con n=0 que me da la lista vacia.
  ;El caso general me dice que le agrego un 0 a todos los otros ceros que ya tengo
  (if (zero? n)
      '()
      (cons 0 (ceros (- n 1)))
  )
)

;De una lista generamos una lista
;Incrementamos en uno el valor de los elementos
(define (incrementa lista)
  (if (null? lista)
      '()
      (cons (+ (car lista) 1) (incrementa (lista cdr)))
   )
 )


;Usado para determinar si hay algun elemento a una profundidad dada
(define (aux-profundidad lista n actual)
  
  (if (null? (cdr lista))
      (if 
  (if (= n actual)
      actual
      (aux-profundidad (car lista) n actual+1)      
  )
)

(define (aux-profundidad-1 lista n)
  (cond
    [(= n (aux-profunidad lista n 1)) #t]
    []
  )
)

(define (profundidad? lista n)
  (if (and (null? lista) (= 0 n))
      #t
      (if (null? lista)
          #f
          (aux-profundidad-1 lista n)
      )
  )
)


;Funcion tabla que crea una tabla con N renglones y M columnas, llena con V
(define (tabla N M V)
)

;Funcion para concatenar todas las sublistas de una lista posiblemente
;imbricada. Debe eliminar todos los elementos que no son sublistas
(define (concatena lista)
)