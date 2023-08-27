#lang stacker/smol/state
;#:no-trace

;El razonamiento detras del programa fue, que se necesitaba un bloque superirior, que contenga
;un bloque mas pequeño en donde se hace la ultima operacion (y), para que se hiciera las operaciones
;del nivel superior, donde estos bloques solo tengan declaraciones locales en sus bloques,
;en donde la respuesta al terminar el programa es nueve

(deffun (f)
    (deffun (g)
      (deffun (pause) 0)
      (defvar y 6)
      (+ (pause) y))
    (defvar x 3)
    (+ (g) x))
(f)
