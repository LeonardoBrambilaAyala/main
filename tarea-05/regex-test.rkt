#lang racket

(require rackunit
         rackunit/text-ui
         "regex.rkt")

(define regex-tests
  (test-suite
   "Pruebas para regex.rkt"
   
   (test-case "open-paren-regex"
              (check-true (regexp-match? open-paren-regex "("))
              (check-false (regexp-match? open-paren-regex ")"))
              (check-false (regexp-match? open-paren-regex "abc")))
   
   (test-case "close-paren-regex"
              (check-true (regexp-match? close-paren-regex ")"))
              (check-false(regexp-match? close-paren-regex "("))
              (check-false (regexp-match? close-paren-regex "abc")))

   (test-case "define-regex"
              (check-true (regexp-match? define-regex "define"))
              (check-false(regexp-match-exact? define-regex "defined")))
   
   (test-case "sum-regex"
              (check-true(regexp-match? sum-regex "+"))
             (check-false(regexp-match? sum-regex "-")))
   
   (test-case "mult-regex"
              (check-true(regexp-match? mult-regex "*"))
              (check-false(regexp-match? mult-regex "+++")))
   
   (test-case "identifier-regex"
              (check-true(regexp-match? identifier-regex "x"))
              (check-true(regexp-match? identifier-regex "y123"))
              (check-false(regexp-match? identifier-regex "123"))
              (check-false(regexp-match-exact? identifier-regex "88z")))
   
   (test-case "number-regex"
              (check-true (regexp-match? number-regex "-123"))
              (check-true (regexp-match? number-regex "+188"))
              (check-false (regexp-match-exact? number-regex "--88"))
              (check-false (regexp-match-exact? number-regex "-55-88"))
              (check-false (regexp-match-exact? number-regex "77-")))))

(run-tests regex-tests 'verbose)

