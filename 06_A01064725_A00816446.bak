#lang racket
;Matrícula1 A01064725
;Matrícula2 A00816446
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