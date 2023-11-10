#lang racket
(require racket/draw)
 (require pict)

(provide bundle)
(provide explode)
(provide list->chunks)
(provide bundle2)
(provide partition)
(provide isort)
(provide insert)
(provide quicksort2)
(provide quicksort3)
(provide smallers2)
(provide largers2)
(provide quicksort4)
(provide bundle3)

(define (unit-string? x)
  (and (string? x)
       (= (string-length x) 1)))


(define (unit-string-list? x)
  (or (null? x)
      (and (pair? x)
           (string? (car x))
           (= (string-length (car x)) 1)
           (unit-string-list? (cdr x)))))


(define (explode s)
  (unless (string? s)
    (error 'explode "esperaba una cadena, pero recibí: ~e" s))
  (map string (string->list s)))

(define (implode ls)
  (unless (unit-string-list? ls)
    (error 'implode "esperaba una lista de cadenas unitarias, pero recibí: ~e"
           ls))
  (apply string-append ls))


(define (bundle s n)
  (if (null? s)
      '()
      (let auxloop ((s s) (aux '()) (res '()))
        (if (null? s)
            (reverse (cons (implode (reverse aux)) res))
            (if (= n (length aux))
                (auxloop s '() (cons (implode (reverse aux)) res))
                (auxloop (cdr s) (cons (car s) aux) res))))))

;; Procedimiento que toma los primeros n elementos de la lista l
(define (take l n)
  (cond
    [(or (empty? l) (zero? n)) '()]  ; Si la lista es vacía o n es 0, devuelve una lista vacía
    [else (cons (car l) (take (cdr l) (sub1 n)))]))  ; Cons el primer elemento y llama recursivamente con el resto de la lista y n decrecido

;; Procedimiento que elimina los primeros n elementos de la lista l
(define (drop l n)
  (cond
    [(or (empty? l) (zero? n)) l]  ; Si la lista es vacía o n es 0, devuelve la lista original
    [else (drop (cdr l) (sub1 n))]))  ; Llama recursivamente con el resto de la lista y n decrecido

;;(define (list->chunks lst n)
 ;; (cond
    ;;((or (null? lst) (zero? n)) '())
    ;;((< n 1) (error "El tamaño de los trozos debe ser al menos 1"))
    ;;(else (cons (take lst (min n (length lst))) 
              ;;  (list->chunks (drop lst n) n)))))

(define (list->chunks l n)
  (cond
    ;;'()
    [(null? l) null]
    [else (cons (take l n) (list->chunks (drop l n) n))]))

(define (bundle2 s n)
  (map implode (list->chunks s n)))

;;(define (partition s n)
 ;; (if (or (string=? s "") (zero? n))
    ;;  '()
     ;; (let loop ((start 0) (end (min n (string-length s))) (result '()))
        ;;(if (>= end (string-length s))
          ;;  (reverse (cons (substring s start) result))
           ;; (loop end (+ end n) (cons (substring s start end) result))))))


(define (partition s n)
  (cond
    [(< (string-length s) n) (list s)]
    [else (cons (substring s 0 n)
                (partition (substring s n) n))]))

(define (isort ls aux)
  (if (empty? ls)
      ;;null
      '()
      (insert (first ls) (isort (rest ls) aux) aux)))

(define (insert n ls aux)
  (cond
    [(empty? ls) (list n)]
    [(aux n (first ls)) (cons n ls)]
    [else (cons (first ls) (insert n (rest ls) aux))]))

(define (smallers ls pivot less?)
  (cond
    [(empty? ls) null]
    [(less? (first ls) pivot)
     (cons (first ls) (smallers (rest ls) pivot less?))]
    [else
     (smallers (rest ls) pivot less?)]))

(define (largers ls pivot greater?)
  (cond
    [(empty? ls) null]
    [(greater? (first ls) pivot)
     (cons (first ls) (largers (rest ls) pivot greater?))]
    [else
     (largers (rest ls) pivot greater?)]))

(define (quicksort2 ls compare)
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort2 (smallers ls pivot compare) compare)
             (list pivot)
             (quicksort2 (largers ls pivot compare) compare))]))

(define (quicksort3 ls compare threshold)
  (define (smallers lst pivot cmp)
    (filter (lambda (x) (cmp x pivot)) lst))
  (define (largers lst pivot cmp)
    (filter (lambda (x) (not (cmp x pivot))) lst))
  
  (cond
    [(empty? ls) null]
    [(< (length ls) threshold)
     (isort ls compare)] ; Usamos isort si la longitud es menor al umbral
    [else
     (define pivot (first ls))
     (append (quicksort3 (smallers ls pivot compare) compare threshold)
             (list pivot)
             (quicksort3 (largers ls pivot compare) compare threshold))]))

(define (smallers2 ls pivot less?)
  (filter (lambda (x) (less? x pivot)) ls))

(define (largers2 ls pivot greater?)
  (filter (lambda (x) (greater? x pivot)) ls))

(define (quicksort4 ls)
  
  (define (smallers3 lst pivot)
    (filter (lambda (x) (< x pivot)) lst))
  
  (define (largers3 lst pivot)
    (filter (lambda (x) (>= x pivot)) lst))
  
  (cond
    [(empty? ls) null]
    [else
     (define pivot (first ls))
     (append (quicksort4 (smallers3 ls pivot))
             (list pivot)
             (quicksort4 (largers3 ls pivot)))]))

(define (bundle3 s n)
  (if (null? s)
      '()
      (let ciclo ((s s) (tam '()) (res '()))
        (if (null? s)
            (reverse (cons (implode (reverse tam)) res))
            (if (= n (length tam))
                (ciclo s '() (cons (implode (reverse tam)) res))
                (ciclo (cdr s) (cons (car s) tam)res))))))

(define (triangle side width color)
  (define w side)
  (define h (* side (sin (/ pi 3))))
  (define (draw-it ctx dx dy)
    (define prev-pen (send ctx get-pen))
    (define path (new dc-path%))
    (send ctx set-pen (new pen% [width width] [color color]))
    (send path move-to 0 h)
    (send path line-to w h)
    (send path line-to (/ w 2) 0)
    (send path close)
    (send ctx draw-path path dx dy)
    (send ctx set-pen prev-pen))
  (dc draw-it w h))

