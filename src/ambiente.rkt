#lang eopl
;Autores: Luis David Mendoza Manzano 2067621

#|
  Taller 3 — Cauca con referencias y tipos.
  TAD ambiente y valor procedimiento.

  Las dos definiciones van en el mismo módulo porque se nombran entre
  sí: el campo del ambiente dentro de una clausura pide ambiente?, y
  apply-env construye una clausura cuando resuelve un nombre ligado por
  letrec. Separarlas deja uno de los dos campos sin predicado.

  ambiente, con tres variantes:
    ambiente-vacio()
    ambiente-extendido(ids, valores, env)
    ambiente-recursivo(proc-names, idss, bodies, env)
      la variante recursiva no construye clausuras: guarda nombres,
      parámetros y cuerpos, y apply-env arma la clausura en el momento de
      la búsqueda con el propio ambiente recursivo (EOPL, figura 3.9).

  procval, con una variante:
    cerradura(ids, body, env)
      los parámetros, el cuerpo sin evaluar y el ambiente donde se creó.

  El cuerpo de una clausura es una expresion?, que viene de sintaxis.rkt.
  Los valores del ambiente son números, booleanos, procedimientos o
  referencias; el ambiente no distingue entre ellos.
|#

(require "sintaxis.rkt")

(define-datatype ambiente ambiente?
  (ambiente-vacio)
  (ambiente-extendido
   (ids (list-of symbol?))
   (valores list?)
   (env ambiente?))
  (ambiente-recursivo
   (proc-names (list-of symbol?))
   (idss list?)
   (bodies (list-of expresion?))
   (env ambiente?)))

(define-datatype procval procval?
  (cerradura
   (ids (list-of symbol?))
   (body expresion?)
   (env ambiente?)))

;; apply-env : ambiente x symbol -> valor
;; Busca la ligadura del identificador, respetando el shadowing de las
;; ligaduras más recientes. Sobre el ambiente vacío levanta eopl:error;
;; sobre el recursivo, si el nombre es uno de los declarados, construye la
;; clausura del procedimiento con el propio ambiente recursivo.
;; Ejemplo: (apply-env (ambiente-inicial) 'n) => 10
(define apply-env
  (lambda (env id)
    (cases ambiente env
      (ambiente-vacio ()
        (eopl:error 'apply-env "La variable ~s no esta ligada" id))
      (ambiente-extendido (ids valores old-env)
        (letrec ((buscar-valor
                  (lambda (ids valores)
                    (cond
                      ((null? ids) (apply-env old-env id))
                      ((equal? (car ids) id) (car valores))
                      (else (buscar-valor (cdr ids) (cdr valores)))))))
          (buscar-valor ids valores)))
      (ambiente-recursivo (proc-names idss bodies old-env)
        (letrec ((buscar-proc
                  (lambda (proc-names idss bodies)
                    (cond
                      ((null? proc-names) (apply-env old-env id))
                      ((equal? (car proc-names) id)
                       (cerradura (car idss) (car bodies) env))
                      (else (buscar-proc (cdr proc-names)
                                         (cdr idss)
                                         (cdr bodies)))))))
          (buscar-proc proc-names idss bodies))))))

;; ambiente-inicial : -> ambiente
;; El del enunciado: n = 10, m = 3, p = 7, q = 2, r = 5.
;; Ejemplo: (apply-env (ambiente-inicial) 'q) => 2
(define ambiente-inicial
  (lambda ()
    (ambiente-extendido '(n m p q r)
                        '(10 3 7 2 5)
                        (ambiente-vacio))))

(provide (all-defined-out))