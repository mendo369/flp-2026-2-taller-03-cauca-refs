#lang eopl
;Autores: Luis David Mendoza Manzano 2067621

#|
  Taller 3 — Cauca con referencias y tipos.
  El tipo y el ambiente de tipos (Parte 3).

  El datatype tipo lo genera la gramática, porque las anotaciones se
  escriben dentro del programa. Aquí van la comparación estructural de
  dos tipos, el texto con que un tipo aparece en los mensajes de error y
  el ambiente de tipos.

  El ambiente de tipos tiene las mismas tres variantes del ambiente de
  valores:
    tenv-vacio()
    tenv-extendido(ids, tipos, env)
    tenv-recursivo(proc-names, tipos-args, tipos-res, env)
  La recursiva guarda el tipo declarado de cada procedimiento (los tipos
  de sus parámetros y el de su resultado), que es lo que permite chequear
  el cuerpo de un letrec sin evaluarlo: apply-tenv arma el tipo
  (t1 * ... * tn -> r) al buscar el nombre.

  El módulo requiere sintaxis.rkt y ningún otro del intérprete. Los
  nombres auxiliares llevan prefijos propios (tipar-, nombre-, texto-)
  para no chocar con los de primitivas.rkt cuando las dos fachadas se
  requieren juntas.
|#

(require "sintaxis.rkt")

(define-datatype tenv tenv?
  (tenv-vacio)
  (tenv-extendido
   (ids (list-of symbol?))
   (tipos (list-of tipo?))
   (env tenv?))
  (tenv-recursivo
   (proc-names (list-of symbol?))
   (tipos-args list?)
   (tipos-res (list-of tipo?))
   (env tenv?)))

;; texto-lista-tipos : lista de tipos -> string
;; Los tipos separados por " * ", como se escriben en la anotación de un
;; procedimiento.
;; Ejemplo: con (num-tipo) y (bool-tipo) => "num * bool"
(define texto-lista-tipos
  (lambda (tipos)
    (cond
      ((null? tipos) "")
      ((null? (cdr tipos)) (tipo->texto (car tipos)))
      (else (string-append (tipo->texto (car tipos))
                           " * "
                           (texto-lista-tipos (cdr tipos)))))))

;; tipo->texto : tipo -> string
;; La forma en que los tipos aparecen en los mensajes de error:
;; "num", "bool", "refto(num)", "(num * num -> bool)". Un procedimiento
;; sin parámetros se escribe "(-> num)".
;; Ejemplo: el tipo (proc-tipo (list (num-tipo) (num-tipo)) (bool-tipo))
;;   => "(num * num -> bool)"
(define tipo->texto
  (lambda (t)
    (cases tipo t
      (num-tipo () "num")
      (bool-tipo () "bool")
      (ref-tipo (t1) (string-append "refto(" (tipo->texto t1) ")"))
      (proc-tipo (args res)
        (if (null? args)
            (string-append "(-> " (tipo->texto res) ")")
            (string-append "(" (texto-lista-tipos args)
                           " -> " (tipo->texto res) ")"))))))

;; tipar-listas-iguales? : lista de tipos x lista de tipos -> boolean
;; #t si las dos listas tienen la misma longitud y sus elementos son
;; iguales por estructura, uno a uno.
;; Ejemplo: (tipar-listas-iguales? (list (num-tipo)) (list (num-tipo))) => #t
(define tipar-listas-iguales?
  (lambda (ts1 ts2)
    (cond
      ((null? ts1) (null? ts2))
      ((null? ts2) #f)
      (else (and (tipos-iguales? (car ts1) (car ts2))
                 (tipar-listas-iguales? (cdr ts1) (cdr ts2)))))))

;; tipos-iguales? : tipo x tipo -> boolean
;; Compara dos tipos por estructura, no por identidad.
;; Ejemplo: (tipos-iguales? (ref-tipo (num-tipo)) (ref-tipo (num-tipo))) => #t
;;          (tipos-iguales? (num-tipo) (bool-tipo)) => #f
(define tipos-iguales?
  (lambda (t1 t2)
    (cases tipo t1
      (num-tipo ()
        (cases tipo t2
          (num-tipo () #t)
          (else #f)))
      (bool-tipo ()
        (cases tipo t2
          (bool-tipo () #t)
          (else #f)))
      (ref-tipo (s1)
        (cases tipo t2
          (ref-tipo (s2) (tipos-iguales? s1 s2))
          (else #f)))
      (proc-tipo (args1 res1)
        (cases tipo t2
          (proc-tipo (args2 res2)
            (and (tipar-listas-iguales? args1 args2)
                 (tipos-iguales? res1 res2)))
          (else #f))))))

;; apply-tenv : tenv x symbol -> tipo
;; Busca el tipo del identificador, respetando el shadowing de las
;; ligaduras más recientes. Si no está declarado levanta eopl:error con
;; «Variable no declarada: <x>». En un tenv-recursivo, si el nombre es
;; uno de los declarados, arma el tipo (t1 * ... * tn -> r) con los tipos
;; guardados.
;; Ejemplo: (apply-tenv (tenv-inicial) 'n) => (num-tipo)
(define apply-tenv
  (lambda (amb id)
    (cases tenv amb
      (tenv-vacio ()
        (eopl:error 'apply-tenv "Variable no declarada: ~s" id))
      (tenv-extendido (ids tipos old-env)
        (letrec ((buscar-tipo
                  (lambda (ids tipos)
                    (cond
                      ((null? ids) (apply-tenv old-env id))
                      ((equal? (car ids) id) (car tipos))
                      (else (buscar-tipo (cdr ids) (cdr tipos)))))))
          (buscar-tipo ids tipos)))
      (tenv-recursivo (proc-names tipos-args tipos-res old-env)
        (letrec ((buscar-proc
                  (lambda (proc-names tipos-args tipos-res)
                    (cond
                      ((null? proc-names) (apply-tenv old-env id))
                      ((equal? (car proc-names) id)
                       (proc-tipo (car tipos-args) (car tipos-res)))
                      (else (buscar-proc (cdr proc-names)
                                         (cdr tipos-args)
                                         (cdr tipos-res)))))))
          (buscar-proc proc-names tipos-args tipos-res))))))

;; tenv-inicial : -> tenv
;; Declara como num las cinco variables del ambiente inicial.
;; Ejemplo: (apply-tenv (tenv-inicial) 'q) => (num-tipo)
(define tenv-inicial
  (lambda ()
    (tenv-extendido '(n m p q r)
                    (list (num-tipo) (num-tipo) (num-tipo) (num-tipo) (num-tipo))
                    (tenv-vacio))))

;; nombre-primitiva : primitiva -> symbol
;; El símbolo con que la primitiva se escribe en el programa; sirve para
;; los mensajes de error.
;; Ejemplo: (nombre-primitiva (sum-prim)) => +
(define nombre-primitiva
  (lambda (prim)
    (cases primitiva prim
      (sum-prim () '+)
      (minus-prim () '-)
      (mult-prim () '*)
      (div-prim () '/)
      (add-prim () 'add1)
      (sub-prim () 'sub1)
      (mayor-prim () '>)
      (mayorigual-prim () '>=)
      (menor-prim () '<)
      (menorigual-prim () '<=)
      (igual-prim () '==)
      (not-prim () 'not)
      (and-prim () 'and)
      (or-prim () 'or)
      (abs-prim () 'abs)
      (mcd-prim () 'mcd)
      (mcm-prim () 'mcm)
      (promedio-prim () 'promedio)
      (xor-prim () 'xor)
      (implica-prim () 'implica)
      (entre-prim () 'entre))))

;; tipar-cantidad : lista x int x symbol -> boolean
;; #t si hay exactamente n tipos de argumento; si no, levanta eopl:error.
;; Ejemplo: (tipar-cantidad (list (num-tipo)) 1 'abs) => #t
(define tipar-cantidad
  (lambda (tipos n quien)
    (if (= (length tipos) n)
        #t
        (eopl:error 'type-of
                    "La primitiva ~s espera ~s argumentos y recibio ~s"
                    quien n (length tipos)))))

;; tipar-minimo : lista x int x symbol -> boolean
;; #t si hay al menos n tipos de argumento; si no, levanta eopl:error.
;; Ejemplo: (tipar-minimo (list (num-tipo) (num-tipo)) 2 'mcd) => #t
(define tipar-minimo
  (lambda (tipos n quien)
    (if (>= (length tipos) n)
        #t
        (eopl:error 'type-of
                    "La primitiva ~s espera al menos ~s argumentos y recibio ~s"
                    quien n (length tipos)))))

;; tipar-todos : lista de tipos x tipo x symbol x int -> boolean
;; #t si cada tipo de la lista es igual a esperado; si no, levanta
;; eopl:error diciendo cuál argumento (contando desde 1) está mal.
;; Ejemplo: (tipar-todos (list (num-tipo)) (num-tipo) '+ 1) => #t
(define tipar-todos
  (lambda (tipos esperado quien i)
    (cond
      ((null? tipos) #t)
      ((tipos-iguales? (car tipos) esperado)
       (tipar-todos (cdr tipos) esperado quien (+ i 1)))
      (else
       (eopl:error 'type-of
                   "El argumento ~s de la primitiva ~s debe ser ~a, se encontro ~a"
                   i quien (tipo->texto esperado) (tipo->texto (car tipos)))))))

;; tipo-de-primitiva : primitiva x lista de tipos -> tipo
;; Lo que cada primitiva exige de sus argumentos y lo que retorna: las
;; aritméticas retornan num; las relacionales, las booleanas y entre
;; retornan bool. Las aridades son las del intérprete.
;; Ejemplo: (tipo-de-primitiva (sum-prim) (list (num-tipo) (num-tipo)))
;;   => (num-tipo)
;;          (tipo-de-primitiva (mayor-prim) (list (num-tipo) (num-tipo)))
;;   => (bool-tipo)
(define tipo-de-primitiva
  (lambda (prim tipos)
    (let ((quien (nombre-primitiva prim)))
      (cases primitiva prim
        (sum-prim ()
          (begin (tipar-todos tipos (num-tipo) quien 1) (num-tipo)))
        (minus-prim ()
          (begin (tipar-minimo tipos 1 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (num-tipo)))
        (mult-prim ()
          (begin (tipar-todos tipos (num-tipo) quien 1) (num-tipo)))
        (div-prim ()
          (begin (tipar-minimo tipos 1 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (num-tipo)))
        (add-prim ()
          (begin (tipar-cantidad tipos 1 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (num-tipo)))
        (sub-prim ()
          (begin (tipar-cantidad tipos 1 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (num-tipo)))
        (mayor-prim ()
          (begin (tipar-cantidad tipos 2 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (bool-tipo)))
        (mayorigual-prim ()
          (begin (tipar-cantidad tipos 2 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (bool-tipo)))
        (menor-prim ()
          (begin (tipar-cantidad tipos 2 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (bool-tipo)))
        (menorigual-prim ()
          (begin (tipar-cantidad tipos 2 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (bool-tipo)))
        (igual-prim ()
          (begin (tipar-cantidad tipos 2 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (bool-tipo)))
        (not-prim ()
          (begin (tipar-cantidad tipos 1 quien)
                 (tipar-todos tipos (bool-tipo) quien 1)
                 (bool-tipo)))
        (and-prim ()
          (begin (tipar-cantidad tipos 2 quien)
                 (tipar-todos tipos (bool-tipo) quien 1)
                 (bool-tipo)))
        (or-prim ()
          (begin (tipar-cantidad tipos 2 quien)
                 (tipar-todos tipos (bool-tipo) quien 1)
                 (bool-tipo)))
        (abs-prim ()
          (begin (tipar-cantidad tipos 1 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (num-tipo)))
        (mcd-prim ()
          (begin (tipar-minimo tipos 2 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (num-tipo)))
        (mcm-prim ()
          (begin (tipar-minimo tipos 2 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (num-tipo)))
        (promedio-prim ()
          (begin (tipar-minimo tipos 1 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (num-tipo)))
        (xor-prim ()
          (begin (tipar-cantidad tipos 2 quien)
                 (tipar-todos tipos (bool-tipo) quien 1)
                 (bool-tipo)))
        (implica-prim ()
          (begin (tipar-cantidad tipos 2 quien)
                 (tipar-todos tipos (bool-tipo) quien 1)
                 (bool-tipo)))
        (entre-prim ()
          (begin (tipar-cantidad tipos 3 quien)
                 (tipar-todos tipos (num-tipo) quien 1)
                 (bool-tipo)))))))

(provide (all-defined-out))