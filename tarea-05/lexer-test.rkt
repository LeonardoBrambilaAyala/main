#lang racket

(require rackunit
         rackunit/text-ui
         "lexer.rkt")


;; Define tus pruebas unitarias
(define (run-tests)
  
  ;; Prueba con un número
  (check-true (not (null? (stream->list (lex-from-string "8"))))
              "Se esperaba una lista no vacía de tokens para el número 8")

  ;; Prueba con una suma
  (check-false (equal? (stream->list (lex-from-string "123 + 456"))
                       '((number 123 1 0)
                         (binop + 1 4)
                         (number 456 1 6))))

  ;; Prueba con un identificador y una definición
  (check-false (equal? (stream->list (lex-from-string "x1 define 6"))
                       '((identifier x1 1 0)
                         (define #f 1 3)
                         (number 6 1 10))))

  ;;Prueba con un binario y un paréntesis
  (check-false (equal? (stream->list (lex-from-string "1011 ("))
                       '((number 1011 1 0)
                         (open-paren #t 1 5)))))



