#lang racket

(define  (impares enteroEntrada)
  (cond ((< enteroEntrada 10) (if (odd? enteroEntrada) 1 0))
        ((even? (remainder enteroEntrada 10)) (impares (quotient enteroEntrada 10)))
        (else ( + 1 (impares (quotient enteroEntrada 10)))))
)


(define (fibo3 n)
  (if (< n 4)
      1
      (+ (fibo3 (- n 1)) (fibo3 (- n 2)) (fibo3 (- n 3))))
)




(define (fibo3t n)
  (fibo3t-aux n 1 1 1)
)

(define (fibo3t-aux n a1 a2 a3)
  (if (< n 4)
      a1
      (fibo3t-aux (- n 1) (+ a1 a2 a3) a1 a2)
      )
)


; Funcion que regresa la ultima celda de una lista
(define lastBox
  (lambda (list)
  (if (null? (cdr list))
      (car list)
      (lastBox (cdr list)))))

; Funcion que regresa la penultima casilla
(define prevToLastBox
  (lambda (list)
  (if (null? (cddr list))
      (car list)
      (prevToLastBox (cdr list)))))

; Funcion que regresa el elemento nesimo de una lista
(define nElement
  (lambda (list n)
      (if(eqv? n 1)
           (car list)
           (nElement (cdr list) (- n 1))
       )
   )
)


; Funcion que encuentra el numero de elementos en una lista
(define numberOfElements
  (lambda (list)
    (if (null? (cdr list))
        1
        (+ 1 (numberOfElements (cdr list)))
    )
  )
)


; Funcion que invierte la lista de entrada 
(define manualReverse
  (lambda (listIn (listOut '()))
    (if (null? listIn)
        listOut
        (manualReverse (cdr listIn) (cons (car listIn) listOut))
    )
  )
)


; Funcion para ver si la lista es o no un palindromo
(define palindrome
  (lambda (list)
    (equal? list (manualReverse list))
  )
)


; Funcion que aplana la lista
(define flattenList
  (lambda (list)
    (cond
      [(null? list) '()]
      [(pair? (car list)) (append (flattenList (car list)) (flattenList(cdr list)))]
      [else (cons (car list) (flattenList (cdr list)))]
      )
    )
)

; Elimina duplicados consecutivos
(define deleteConsecutives
  (lambda (list)
    (cond
      [(null? list) '()]
      [(null? (cdr list)) list]
      [(eq? (car list)(cadr list)) (deleteConsecutives (cdr list))]
      [else (cons (car list) (deleteConsecutives (cdr list)))]
    )
  )
)



  
; Agrupa iguales consecutivos en sublistas
; Preguntar a profesor
(define groupUp
  (lambda (list (listOut '()))
    (cond
      [(null? list) listOut]
      [(null? (cdr list)) (cons (car list) (car listOut))]
      [(eq? (car list) (cadr list)) (groupUp (cdr list) (append listOut (car list)))]
      [else (groupUp (cdr list) (cons (cons (car list) '()) listOut))]
      )
    )
  )





(define dupli
  (lambda (list)
    (if (null? list)'()
        ;Preguntar acerca de lo que regresa esta funcion
        (cons (car list) (cons (car list) (dupli(cdr list)))))
    )
  )


(define repeat
  (lambda (val n)
    (if (zero? n) '()
        (cons val (repeat val (- n 1)))
    )
  )
)
; Funcion para replicar elementos de una lista un numero x de veces
(define repli
  (lambda (list n)
    ; Si la lista es nula regresamos vacio para terminar
    (if (null? list) '()
        ; Si no es vacio llamamos a repeat para que haga las inserciones
        ; Y llamamos esta misma funcion con el cdr para que tome el sig elemento
        (append (repeat (car list) n) (repli (cdr list) n)))
  )
)


; Descartar elemento n
; Duda si forma correcta
(define dropN
  (lambda (list n (listOut '()))
    (cond
      [(null? list) listOut]
      [(eq? n 1) (append (reverse listOut) (cdr list))]
      [else (dropN (cdr list) (- n 1) (cons (car list) listOut))]
    )
  )
)

; Parte una lista en dos partes
(define split
  (lambda (listSec n (listFirst '()))
    (if (or (null? listSec) (zero? n))
        (list (reverse listFirst) listSec)
        (split (cdr listSec) (- n 1) (cons (car listSec) listFirst))) 
  )
)

; Inserta un elemento en la posicion dada
(define insert
  (lambda (new list pos)
    (cond
      [(zero? pos) list]
      [(eq? pos 1) (cons new list)]
      [(null? list) '()]
      [else (cons (car list) (insert new (cdr list) (- pos 1)))]
    )
  )
)

; Crea una lista en un rango predeterminado
(define listInRange
  (lambda (init end)
    (if (eq? init end)
        '()
         (cons ( + 1 init) (listInRange (+ 1 init) end))
    )
  )
)

