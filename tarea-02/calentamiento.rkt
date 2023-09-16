#lang racket

;1
(define pi 3.14)
;2
  (define (area-circle radio)
    (* pi (* radio radio)))

(define (circle-properties radio)
  (list (* pi (* radio radio)) (* 2 pi radio)))

  (define (rectangle-properties aux)
    (define largo (car aux))
    (define ancho (cadr aux))
    (define area (* largo ancho))
    (define perimetro (* 2 (+ largo ancho)))
    (list area perimetro))

  (define (find-needle t)
    (cond ((eq? (car t) 'needle) 0)
          ((eq? (cadr t)'needle) 1)
          (else -1)))

  (define (abs x)
  (if (positive? x) x
      (* -1 x)))


  (define another-add
    (lambda (n m)
      (cond
        [(zero? n) m]
        [else (add1 (another-add (sub1 n) m))])))