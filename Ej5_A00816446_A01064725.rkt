#lang scheme
(define pi 3.1416)
(define (circulo r p)
  (cond
    ((eq? p 'diametro)  (* r 2))
    ((eq? p 'perimetro) (* pi r 2))
    ((eq? p 'area)      (* pi r r))
    ))
(define (mayorDeDos x y)
  (if (> x y)
    x
    y
  ))
(define (mayor a b c d)
  (mayorDeDos a (mayorDeDos b (mayorDeDos c d)))
  )
(define (salida_paronon x)
  (cond
    ((< x 2) 'pares)
    ((= x 2) 'empate)
    (else    'nones)
    ))
(define (paronon a b c d)
  (salida_paronon
    (+ (modulo a 2) (modulo b 2) (modulo c 2) (modulo d 2))
  ))