#lang stacker/smol/state

;Para este programa se utilizaron cuatro bloques, para asi tener las operaciones en sus respectivos
; bloques como la suma y las declaraciones de variables dentro del mismo bloque en donde se llaman;
;para que utilizen es vinculacion y no la de un bloque superior y se utilizo pause, para qeu continuara sin
;error al siguiente bloque en donde contiene una misma definicion de variable en el siguiente bloque

(deffun (Cuarto)
  (deffun (Tercero)
    (deffun (f)
      (deffun (g)
        (deffun (pause) 0)
        (defvar x 2)
        (pause))
      (defvar x 4)
      (+(g) x))
    (defvar y 5)
    (+ 4 (f) y))
  (defvar x 3)
  (+ (Tercero) x))
(Cuarto)

