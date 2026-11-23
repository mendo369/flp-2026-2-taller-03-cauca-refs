#lang eopl
;Autores: Luis David Mendoza Manzano 2067621

#|
  Taller 3 — Cauca con referencias y tipos.
  TAD referencia y almacén (Parte 1).

  El módulo no requiere ningún otro: el almacén guarda valores
  cualesquiera y no necesita la gramática.

  El almacén es una lista de celdas indexadas por posición, guardada en
  una variable global, the-store. Es el único lugar del curso donde se
  usa set!, y solo dentro de initialize-store!, newref! y setref!.

  Invariante que las operaciones preservan, y que se demuestra en la
  Parte 5:
    1. toda referencia entregada por newref! apunta a una celda existente
    2. el almacén solo crece por newref!
    3. setref! cambia una celda y deja las demás intactas
    4. deref sobre una referencia fuera de rango levanta eopl:error
|#

;; referencia : TAD
;; Una referencia nombra una celda por su posición. Es un tipo abstracto y
;; no un entero desnudo, para que un programa de Cauca no pueda hacer
;; aritmética con posiciones de memoria.
(define-datatype referencia referencia?
  (a-ref (pos integer?)))

;; the-store : lista de valores
;; El almacén: la celda de la posición i es el elemento i de la lista.
(define the-store '())

;; empty-store : -> almacen
;; El almacén sin celdas.
;; Ejemplo: (empty-store) => ()
(define empty-store
  (lambda ()
    '()))

;; initialize-store! : -> void
;; Deja el almacén vacío. Se llama al empezar cada programa.
;; Ejemplo: después de (initialize-store!), (tamano-almacen) => 0
(define initialize-store!
  (lambda ()
    (set! the-store (empty-store))))

;; tamano-almacen : -> int
;; La cantidad de celdas que tiene el almacén ahora.
;; Ejemplo: tras dos llamadas a newref! sobre un almacén vacío => 2
(define tamano-almacen
  (lambda ()
    (length the-store)))

;; contenido-almacen : -> lista de valores
;; Una vista del almacén: la lista de sus celdas en orden de posición.
;; Sirve para observar el estado en las pruebas y en los informes.
;; Ejemplo: (initialize-store!) (newref! 7) (contenido-almacen) => (7)
(define contenido-almacen
  (lambda ()
    the-store))

;; posicion-valida? : int -> boolean
;; #t si la posición nombra una celda existente del almacén.
(define posicion-valida?
  (lambda (pos)
    (and (>= pos 0) (< pos (length the-store)))))

;; lista-con-valor : lista x int x valor -> lista
;; La misma lista con el elemento de la posición pos reemplazado por val.
;; Las demás posiciones conservan su valor. Requiere 0 <= pos < largo.
;; Ejemplo: (lista-con-valor '(a b c) 1 'x) => (a x c)
(define lista-con-valor
  (lambda (lst pos val)
    (if (= pos 0)
        (cons val (cdr lst))
        (cons (car lst) (lista-con-valor (cdr lst) (- pos 1) val)))))

;; newref! : valor -> referencia
;; Agrega una celda con el valor al final del almacén y retorna la
;; referencia que la nombra. No toca las celdas que ya existían.
;; Ejemplo: sobre un almacén vacío, (newref! 7) => (a-ref 0)
(define newref!
  (lambda (val)
    (let ((pos (length the-store)))
      (begin
        (set! the-store (append the-store (list val)))
        (a-ref pos)))))

;; deref : referencia -> valor
;; Lee la celda que nombra la referencia. No modifica el almacén. Si la
;; posición está fuera de rango levanta eopl:error.
;; Ejemplo: tras (newref! 7), (deref (a-ref 0)) => 7
(define deref
  (lambda (ref)
    (cases referencia ref
      (a-ref (pos)
        (if (posicion-valida? pos)
            (list-ref the-store pos)
            (eopl:error 'deref "Referencia fuera de rango: ~s" pos))))))

;; setref! : referencia x valor -> void
;; Escribe el valor en la celda que nombra la referencia y deja las demás
;; intactas; el almacén no cambia de tamaño. Si la posición está fuera de
;; rango levanta eopl:error.
;; Ejemplo: tras (newref! 7), (setref! (a-ref 0) 9) deja el almacén en (9)
(define setref!
  (lambda (ref val)
    (cases referencia ref
      (a-ref (pos)
        (if (posicion-valida? pos)
            (set! the-store (lista-con-valor the-store pos val))
            (eopl:error 'setref! "Referencia fuera de rango: ~s" pos))))))

(provide (all-defined-out))