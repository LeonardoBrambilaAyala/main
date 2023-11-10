#lang racket

(require rackunit
         rackunit/text-ui
         "ps.rkt")

(displayln "Primero")

; Casos de prueba
(check-equal? (bundle (explode "abcdefgh") 2)
              '("ab" "cd" "ef" "gh"))

(check-equal? (bundle (explode "abcdefg") 3)
              '("abc" "def" "g"))

(check-equal? (bundle '("a" "b") 3)
              '("ab"))

(check-equal? (bundle '() 3)
              '())

(check-equal? (bundle (explode "abcdefg") 3)
              (list "abc" "def" "g"))

(displayln "Segundo")
(displayln "Segundo-take")
(check-equal? (take '(1 2 3 4 5) 3) '(1 2 3))
(check-equal? (take '(10 20 30 40 50) 0) '())

(display "Pruebas para el procedimiento 'drop'")
(check-equal? (drop '(1 2 3 4 5) 2) '(3 4 5))
(check-equal? (drop '(10 20 30 40 50) 0) '(10 20 30 40 50))
;;(check-equal? (drop '(a b c d e) 10) '())

(displayln "Quinto")
(displayln "list->chumks")
(check-equal? (list->chunks '(1 2 3 4 5 6 7 8 9) 3) '((1 2 3) (4 5 6) (7 8 9)))
(check-equal? (list->chunks '(a b c d e f g h i j) 2) '((a b) (c d) (e f) (g h) (i j)))
(check-equal? (list->chunks '(x y z) 1) '((x) (y) (z)))

(displayln "bundle con list->chumks")
;; Pruebas para el procedimiento 'bundle2'
(check-equal?(bundle2 '() 1) '())

(displayln "Patition")
(partition "hola mundo" 3)
(equal? (partition "Adios" 3) (bundle (explode "Adios") 3))

(displayln "Pruebas isort e insert")
(equal? (isort '(3 1 4 1 5 9 2 6 5 3) <) '(1 1 2 3 3 4 5 5 6 9))
(equal? (isort '(2) <) '(2))
(equal? (isort '() <) '())


(equal? (insert 3 '(1 2 4) <) '(1 2 3 40))
(equal? (insert 2 '(1 3 4) <) '(1 5 3 4))
(equal? (insert 4 '(1 2 3) <) '(1 2 3 4))
(equal? (insert 1 '() <) '(2))
(equal? (insert 1 '(2) <) '(1 2))

(displayln "Problema 9 y 11")
;;(check-equal? (quicksort2 '(1 2 3 4 5) less?) '(1 2 3 4 5))

(equal? (quicksort2 '(3 1 4 1 5 9 2 6 5 3 5) <) '(1 1 2 3 3 4 5 5 5 6 9))
(equal? (quicksort2 '(9 8 7 6 5 4 3 2 1) >) '(9 8 7 6 5 4 3 2 1))
(equal? (quicksort2 '(5 4 3 2 1) <) '(1 2 3 4 5))
(equal? (quicksort2 '(1) <) '(1 2 3 4 5))
(equal? (quicksort2 '(5 4 3 2 1) <) '())
(equal? (quicksort2 '() <) '())

(displayln "Problema 12")
(equal? (quicksort3 '() < 5) null) ; Lista vacía
(equal? (quicksort3 '(5 4 6 2 9 1 7 3) < 9) '(1 2 3 4 5 6 7 9)) ; Lista desordenada
(equal? (quicksort3 '(5 4 3 1 9) < 6) '(5 4 1 3 9)) ; Lista con elementos duplicados
(equal? (quicksort3 '(10 20 30 40 50) < 9) '(10 20 30 40 50)) ; Lista ordenada
(equal? (quicksort3 '(50 40 30 20 10) < 7) '()) ; Lista ordenada inversamente
(equal? (quicksort3 '() < 0) '(10 20 30 40 50))

(displayln "Problema 13")
;; Pruebas unitarias para smallers
(check-equal? (smallers2 '(3 1 4 1 5 9 2 6 5 3 5) 5 <) '(3 1 4 1 2 3))
(check-equal? (smallers2 '(10 20 30 40 50) 35 <) '(10 20 30))
(check-equal? (smallers2 '(5 5 5 5 5) 8 <) '(5 5 5 5 5))

;; Pruebas unitarias para largers
(check-equal? (largers2 '(3 1 1 1 3 9 2 6 5 3 5) 4 >) '(9 6 5 5))
(check-equal? (largers2 '(10 20 30 40 50) 35 >) '(40 50))
(check-equal? (largers2 '(5 5 5 5 5) 5 >) '())


(displayln "Problema 16")
(define test-bundle3-1 '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l"))
(define test-bundle3-2 '("1" "2" "3" "4" "5"))
(define test-bundle3-3 '("x" "y" "z" "u" "v" "w"))
(define test-bundle3-4 '())
  
(check-equal? (bundle3 test-bundle3-1 3) '("abc" "def" "ghi" "jkl"))
(check-equal? (bundle3 test-bundle3-2 2) '("12" "34" "5"))
(check-equal? (bundle3 test-bundle3-3 1) '("x" "y" "z" "u" "v" "w"))
(check-equal? (bundle3 test-bundle3-4 1) '())