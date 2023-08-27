#lang stacker/smol/state

;En este programa se utilizan tres bloques, con definiciones de variables dentro de estos bloques
; para que se vinculen a esta svariables y no se vinculen con otra definicion de a misma variable
; en otro bloque, para hacer las operacione de un valor de defido, (el cuadrito en blanco)
; se utilizo "pause" y el nombre de las otras funciones

(deffun (Tercero)
  (deffun (f)
    (deffun (g)
      (deffun (pause) 0)
      (defvar x 4)
      (-(pause) (- x 2)))
    (defvar y 6)
    (+ (g) (+ y 4)))
  (defvar x 3)
  (+ (f) x))
(Tercero)



