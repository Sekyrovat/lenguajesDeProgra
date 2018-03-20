#lang racket
; EJERCICIOS SOBRE PROGRAMACIÓN BÁSICA EN SCHEME

; PROBLEMA 1
; recibe 1 argumento numérico representando su radio y un símbolo que
; especifica una propiedad a calcular (perímetro, área y diámetro) y
; regresa el valor de esa propiedad.
(define (circulo radio propiedad)
  (cond ((eq? propiedad 'perimetro) (* 2 pi radio))
        ((eq? propiedad 'area) (* pi radio radio))
        ((eq? propiedad 'diametro) (* 2 radio))
        (else "Error: propiedad desconocida")))
        
; PROBLEMA 2
; recibe 4 argumentos numéricos y regresa el mayor.
(define (mayor a b c d)
  (cond ((and (>= a b) (>= a c) (>= a d)) a)
        ((and (>= b c) (>= b d)) b)
        ((>= c d) c)
        (else d)))

; PROBLEMA 3
; recibe 4 argumentos numéricos y regresa un símbolo que indique si
; hay más pares, más nones o un empate.
(define (paronon a b c d)
  (let ((cuenta (+ (if (even? a) 1 -1)
                   (if (even? b) 1 -1)
                   (if (even? c) 1 -1)
                   (if (even? d) 1 -1))))
    (cond ((> cuenta 0) 'pares)
          ((< cuenta 0) 'nones)
          (else 'empate))))