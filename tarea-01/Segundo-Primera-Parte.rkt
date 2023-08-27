#lang stacker/smol/state
;#:no-trace

;Se crean dos vectores "v1" y "v2", en donde el primer vector tiene el valor de 1, y el vector 2, tiene la
;direccion  del vector 1 "@100", para hacer que el vector 2 tenga la refencia del vector 1,
;se llamo por refencia en el indice 0,  y despues se mutara en el indece 0 del vector 0, y por ultimo
;para que el stack tenga su ultimo #<void> se utilizo set! en el vector uno para que devolviera un vacio

(deffun (pause) 0)

(defvar v1(mvec 1))

(defvar v2(mvec v1))

(vec-set! (vec-ref v2 0) 0 v2)
(set! v1 0)

(pause)

