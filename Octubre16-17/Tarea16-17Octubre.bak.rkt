#lang racket

(define (empty-env) '())

(define (apply-env env var)
  (if (equal?  (car(first env)) var)
      (cdr(first env))
      (apply-env (rest env) var)))
  

(define (extend-env var val env)
  (cons (cons var val) env))

(define (eval-def var exp env)
  (let ([val (eval-exp exp env)])
    (extend-env var val env)))

(define (eval-prog exps env)
  (cond
    [(null? exps) (void)]
    [(pair? exps) (begin (eval-exp (car exps) env)
                         (eval-prog (cdr exps) env))]
    [else (eval-exp exps env)]))

(define (eval-exp exp env)
  (cond
    [(integer? exp) exp]
    [(symbol? exp) (apply-env env exp)]
    [(and (list? exp)
          (=(length exp)3)
          (eq?(first exp) '+))
     (let*([exp1(second exp)]
           [exp2(third exp)]
           [x1(eval-exp exp1 env)]
           [x2 (eval-exp exp2 env)])
       (+ x1 x2))]
    [(and (list? exp)
          (=(length exp)3)
          (eq?(first exp) '*))
     (let*([exp1(second exp)]
           [exp2(third exp)]
           [x1(eval-exp exp1 env)]
           [x2 (eval-exp exp2 env)])
       (* x1 x2))]))


;;EJEMPLO PARA ENTENDERME A MI MISMO

;;(define initial-env (empty-env)) ; Se crea un entorno vacio

;;(define new-env (eval-def 'x (+ 2 3) initial-env))
;;(define result (eval-exp 'x new-env))

;;(display result)
  