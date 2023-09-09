#lang racket

(define (Primero)
  (define (countdown x)
    (if (< x 0)
        '()
        (cons x (countdown(- x 1)))))

  (display "Primero: ")
  (display (countdown 5)))
(Primero)
(newline)


(define (Segundo)
  (define (insertL simb-1 simb-2 lst)
    (define (insertL-2 x lst)
      ;Pares y listas foldl
      ;Utilizar lambda
      (foldl (lambda (elementos aux)
               (if (eqv? elementos simb-1)
                   (cons simb-2 (cons elementos aux))
                   (cons elementos aux)))
             '() lst))
    (insertL-2 simb-1 lst))

  (define f (insertL 'x 'y '(x z z x y x)))
  (display "Segundo: ")
  (display f))
(Segundo)
(newline)

(define (Tercero)
  (define (remv-lst simbolo lst)
    (cond
      [(null? lst) '()]
      [(eq? (car lst) simbolo) (cdr lst)]
      [else (cons (car lst)
                  (remv-lst simbolo (cdr lst)))]))
  (display "Tercero: ")
  (display (remv-lst 'x '(x y z x))))
(Tercero)
(newline)

(define (Cuarto)
  (define (map p ls)
    (cond
      [(null? ls) '()]
      [else (cons (p (car ls)) (map p (cdr ls)))]))
  (define (aux x)
    (- x 1))
  (define lista '(1 2 3 4))
  (display "Cuarto: ")
  (display (map aux lista)))
(Cuarto)
(newline)

(define (Quinta)
  (define (filter predicado lst)
    (cond
      ((null? lst) '())
      ((predicado (car lst))
       (cons (car lst) (filter predicado (cdr lst))))
      (else
       (filter predicado (cdr lst)))))
  (display "Quinto: ")
  (display (filter even? '(1 2 3 4 5 6))))
(Quinta)
(newline)

(define (Sexta)
  (define (zip lst-1 lst-2)
    (cond
      [(or (null? lst-1) (null? lst-2)) '()]
      [else
       (cons (cons (car lst-1) (car lst-2))
             (zip (cdr lst-1) (cdr lst-2)))]))
  (display "Sexto: ")
  (newline)
  (display (zip '(1 2 3) '(a b c)))
  (newline)
  (display (zip '(1 2 3 4 5 6) '(a b c)))
  (newline)
  (display (zip '(1 2 3) '(a b c d e f)))
  (newline))
(Sexta)

(define (Septima)
  (define (list-index-ofv elemento lst)
    (define (aux elemento lst index)
      (cond
        [(null? lst) #f]
        [(equal? elemento (car lst)) index]
        [else (aux elemento (cdr lst) (+ index 1))]))
    (aux elemento lst 0))

  (display "Septimo: ")
  (newline)
  (display (list-index-ofv 'x '(x y z x x)))
  (newline)
  (display (list-index-ofv 'x '(y z x x))))
(Septima)
(newline)

(define (Octava)
  (define (append ls1 ls2)
    (cond
      [(null? ls1) ls2]
      [(cons (car ls1) (append (cdr ls1) ls2))]))
  
  (display "Octavo: ")
  (newline)
  (display (append '(42 120) '(1 2 3)))
  (newline)
  (display (append '(a b c) '(cat dog))))
(Octava)
(newline)

(define (Novena)
  (define (reverse lst)
    (if (null? lst) '()
      (append (reverse (cdr lst))
              (list (car lst)))))
  (display "Noveno: ")
  (display (reverse '(a 3 x))))
(Novena)
(newline)

(define (Decima)
(define (repeat lst n)
  (define (aux lst n x)
    (if (= n 0) x
        (aux lst (- n 1) (append x lst))))
  (aux lst n '()))
  
  (define y '(4 8 11))
  (define cantidad 4)
  (define w (repeat y cantidad))
  (display "Decimo: ")
  (display w))
(Decima)
(newline)

(define (Onceava)
  (define (same-lists* lst-1 lst-2)
    (if (and (null? lst-1) (null? lst-2)) #t
        (if (or (null? lst-1) (null? lst-2)) #f
            (if (equal? (car lst-1) (car lst-2))
                (same-lists* (cdr lst-1) (cdr lst-2)) #f))))

  (display "Onceavo: ")
  (newline)
  (display (same-lists* '() '()))
  (newline)
  (display (same-lists* '(1 2 3 4 5) '(1 2 3 4 5)))
  (newline)
  (display (same-lists* '(1 2 3 4) '(1 2 3 4 5)))
  (newline)
  (display (same-lists* '(a (b c) d) '(a (b) c d))))
(Onceava)
(newline)

        
