#lang racket

;///////////////////////////////////////////////////////////////////////////////////////////////
  (define (countdown x)
    (if (< x 0)
        '()
        (cons x (countdown(- x 1)))))
;///////////////////////////////////////////////////////////////////////////////////////////////
(define (insertL sym1 sym2 lst)
  (cond
    ((null? lst) '())
    ((eqv? (car lst) sym1)
     (cons sym2 (cons (car lst) (insertL sym1 sym2 (cdr lst)))))
    (else
     (cons (car lst) (insertL sym1 sym2 (cdr lst))))))

(define lst '(x z z x y x))
(define sym1 'x)
(define sym2 'y)

(define result (insertL sym1 sym2 lst))

;///////////////////////////////////////////////////////////////////////////////////////////////
  (define (remv-1st simbolo lst)
    (cond
      [(null? lst) '()]
      [(eq? (car lst) simbolo) (cdr lst)]
      [else (cons (car lst)
                  (remv-1st simbolo (cdr lst)))]))
;//////////////////////////////////////////////////////////////////////////////////////////////
  (define (map p ls)
    (cond
      [(null? ls) '()]
      [else (cons (p (car ls)) (map p (cdr ls)))]))
  (define (aux x)
    (- x 1))
  ;(define lista '(1 2 3 4))
;//////////////////////////////////////////////////////////////////////////////////////////////
  (define (filter predicado lst)
    (cond
      ((null? lst) '())
      ((predicado (car lst))
       (cons (car lst) (filter predicado (cdr lst))))
      (else
       (filter predicado (cdr lst)))))
  ;(define x12 (filter even? '(1 2 3 4 5 6)))
;//////////////////////////////////////////////////////////////////////////////////////////////
  (define (zip lst-1 lst-2)
    (cond
      [(or (null? lst-1) (null? lst-2)) '()]
      [else
       (cons (cons (car lst-1) (car lst-2))
             (zip (cdr lst-1) (cdr lst-2)))]))
  ;(define x7 (zip '(1 2 3) '(a b c)))
;//////////////////////////////////////////////////////////////////////////////////////////////
  (define (list-index-ofv elemento lst)
    (define (aux elemento lst index)
      (cond
        [(null? lst) #f]
        [(equal? elemento (car lst)) index]
        [else (aux elemento (cdr lst) (+ index 1))]))
    (aux elemento lst 0))
;//////////////////////////////////////////////////////////////////////////////////////////////////////////
  (define (append ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(cons (car ls1) (append (cdr ls1) ls2))]))
;///////////////////////////////////////////////////////////////////////////////////////////////////////
  (define (reverse lst)
    (if (null? lst) '()
      (append (reverse (cdr lst))
              (list (car lst)))))
  (define x13 (reverse '(a 3 x)))
;////////////////////////////////////////////////////////////////////////////////////////////////////////
(define (repeat lst n)
  (define (aux lst n x)
    (if (= n 0) x
        (aux lst (- n 1) (append x lst))))
  (aux lst n '()))
;///////////////////////////////////////////////////////////////////////////////////////////////////////
  (define (same-lists* lst-1 lst-2)
    (if (and (null? lst-1) (null? lst-2)) #t
        (if (or (null? lst-1) (null? lst-2)) #f
            (if (equal? (car lst-1) (car lst-2))
                (same-lists* (cdr lst-1) (cdr lst-2)) #f))))
;///////////////////////////////////////////////////////////////////////////////////////////////
(define expr '((w x) y (z)))

(define (puntw lst)
  (if (pair? lst)
      (cons (puntw (car lst)) (puntw (cdr lst)))
      '()))

(define nueva-expr (puntw expr))
;////////////////////////////////////////////////////////////////////////////////////////////
(define (binary->natural lst)
  (define (auxBin lst a b)
    (cond
      ((null? lst) b)
      ((= (car lst) 0)
       (auxBin (cdr lst) (+ a 1) b))
      ((= (car lst) 1)
       (auxBin (cdr lst) (+ a 1) (+ b (expt 2 a))))))
  (auxBin lst 0 0))
;//////////////////////////////////////////////////////////////////////////////////////////
(define (div c d)
  (cond (( < c d) 0)
        ((= c d) 1)
        (else (+ 1 (div (- c d) d)))))
;/////////////////////////////////////////////////////////////////////////////////////////
(define (append-map aux lst)
  (if (empty? lst) '()
      (append (aux (first lst))
              (append-map aux (rest lst)))))


