#lang eopl
;Autores: Luis David Mendoza Manzano 2067621

#|
  Taller 3 — Cauca con referencias y tipos.
  Reglas de tipado y chequeo del programa (Parte 3).

  type-of recorre el árbol con cases sobre un ambiente de tipos y no
  evalúa nada: una regla por variante de expresión, tal como están
  escritas en el cuadro del enunciado.

  Los mensajes de error se distinguen uno de otro. Son los nueve del
  cuadro «Mensajes de error», con ese texto:

    La condicion del if debe ser bool, se encontro <t>
    Las ramas del if deben coincidir: <t1> vs <t2>
    Se intento aplicar un valor de tipo <t>
    El procedimiento espera <n> argumentos y recibio <k>
    El argumento <i> debe ser <t1>, se encontro <t2>
    deref espera refto(t), se encontro <t>
    La celda guarda <t1> y se intento escribir <t2>
    El procedimiento <f> declara <t1> y su cuerpo da <t2>
    Variable no declarada: <x>      (la levanta apply-tenv, en tipos.rkt)

  Además hay mensajes para lo que el cuadro no cubre: setref y swap sobre
  algo que no es una referencia, swap entre celdas de tipos distintos,
  applyref con un argumento que no es refto, el case con control no
  numérico o con ramas de tipos distintos, y las primitivas (en
  tipos.rkt).

  El módulo requiere sintaxis.rkt y tipos.rkt, y ningún otro módulo del
  intérprete: el árbol que recorre es el que produjo el parser, con las
  anotaciones de tipo adentro.

  Ejemplos (el resultado va después de =>; se comprueban en
  pruebas-cauca-refs.rkt con tipo-de-texto):

    Bien tipados
    "proc (a : num, b : num) +(a, b) end"          => "(num * num -> num)"
    "let celda = newref(0) in
       begin setref(celda, 42); deref(celda) end end"    => "num"
    "newref(newref(true))"                          => "refto(refto(bool))"
    "let a = newref(1) b = newref(2) in swap(a, b) end"  => "num"
    "letrec num fact (k : num) =
       if ==(k, 0) then 1 else *(k, apply fact (sub1(k))) end
     in apply fact (5) end"                         => "num"
    "let x = newref(1) in
       let f = proc (u : refto(num)) deref(u) end
       in applyref f (x) end end"                   => "num"
    "case n of 1 => 10 of 2 => 20 otherwise => 0 end"    => "num"
    "proc (f : (num -> num), x : num) apply f (x) end"
                                       => "((num -> num) * num -> num)"

    Mal tipados (el mensaje va después de =>)
    "if n then 1 else 2 end"
      => La condicion del if debe ser bool, se encontro num
    "if >(n, m) then 1 else false end"
      => Las ramas del if deben coincidir: num vs bool
    "apply 5 (1)"
      => Se intento aplicar un valor de tipo num
    "let f = proc (a : num, b : num) +(a, b) end in apply f (1) end"
      => El procedimiento espera 2 argumentos y recibio 1
    "let f = proc (a : num) a end in apply f (true) end"
      => El argumento 1 debe ser num, se encontro bool
    "deref(5)"
      => deref espera refto(t), se encontro num
    "let celda = newref(0) in setref(celda, true) end"
      => La celda guarda num y se intento escribir bool
    "letrec num f (k : num) = true in apply f (1) end"
      => El procedimiento f declara num y su cuerpo da bool
    "z"
      => Variable no declarada: z
|#

(require "sintaxis.rkt")
(require "tipos.rkt")

;; tipar-rands : lista de expresiones x tenv -> lista de tipos
;; El tipo de cada expresión, de izquierda a derecha, en el mismo ambiente.
;; Ejemplo: (tipar-rands (list (lit-exp 1) (true-exp)) (tenv-inicial))
;;   => lista con (num-tipo) y (bool-tipo)
(define tipar-rands
  (lambda (rands amb)
    (if (null? rands)
        '()
        (let ((primero (type-of (car rands) amb)))
          (cons primero (tipar-rands (cdr rands) amb))))))

;; tipar-argumentos : lista de tipos x lista de tipos x int -> boolean
;; Compara los tipos de los argumentos con los de los parámetros, de
;; izquierda a derecha; i es la posición del primer argumento (desde 1).
;; Si alguno no coincide, levanta eopl:error diciendo cuál.
;; Ejemplo: (tipar-argumentos (list (num-tipo)) (list (num-tipo)) 1) => #t
(define tipar-argumentos
  (lambda (esperados encontrados i)
    (cond
      ((null? esperados) #t)
      ((tipos-iguales? (car esperados) (car encontrados))
       (tipar-argumentos (cdr esperados) (cdr encontrados) (+ i 1)))
      (else
       (eopl:error 'type-of
                   "El argumento ~s debe ser ~a, se encontro ~a"
                   i
                   (tipo->texto (car esperados))
                   (tipo->texto (car encontrados)))))))

;; tipar-aridad : lista x lista -> boolean
;; #t si las dos listas tienen la misma longitud; si no, levanta
;; eopl:error con la cantidad esperada y la recibida.
;; Ejemplo: (tipar-aridad (list (num-tipo)) (list (num-tipo))) => #t
(define tipar-aridad
  (lambda (parametros argumentos)
    (if (= (length parametros) (length argumentos))
        #t
        (eopl:error 'type-of
                    "El procedimiento espera ~s argumentos y recibio ~s"
                    (length parametros) (length argumentos)))))

;; tipar-argumentos-ref : lista de tipos x lista de tipos x int -> boolean
;; Para applyref: cada argumento debe tener tipo refto(s) y coincidir con
;; el tipo del parámetro. Si no es refto, levanta eopl:error; si es un
;; refto distinto del parámetro, levanta el error de argumento equivocado.
;; Ejemplo: (tipar-argumentos-ref (list (ref-tipo (num-tipo)))
;;                                (list (ref-tipo (num-tipo))) 1) => #t
(define tipar-argumentos-ref
  (lambda (esperados encontrados i)
    (cond
      ((null? esperados) #t)
      ((not (ref-tipo? (car encontrados)))
       (eopl:error 'type-of
                   "applyref espera refto(t) en el argumento ~s, se encontro ~a"
                   i (tipo->texto (car encontrados))))
      (else
       (begin
         (tipar-argumentos (list (car esperados)) (list (car encontrados)) i)
         (tipar-argumentos-ref (cdr esperados) (cdr encontrados) (+ i 1)))))))

;; ref-tipo? : tipo -> boolean
;; #t si el tipo es de la forma refto(t).
;; Ejemplo: (ref-tipo? (ref-tipo (num-tipo))) => #t
(define ref-tipo?
  (lambda (t)
    (cases tipo t
      (ref-tipo (s) #t)
      (else #f))))

;; tipar-cuerpos-letrec : lista x lista x lista x lista x tenv -> boolean
;; Para cada procedimiento declarado en un letrec, chequea su cuerpo en el
;; ambiente recursivo extendido con sus parámetros y verifica que dé el
;; tipo de resultado declarado. Si no, levanta eopl:error.
;; Ejemplo: sin procedimientos, (tipar-cuerpos-letrec '() '() '() '() '() amb)
;;   => #t
(define tipar-cuerpos-letrec
  (lambda (tipos-res proc-names idss tipos-args bodies amb-rec)
    (if (null? proc-names)
        #t
        (let ((tipo-cuerpo
               (type-of (car bodies)
                        (tenv-extendido (car idss) (car tipos-args) amb-rec))))
          (if (tipos-iguales? tipo-cuerpo (car tipos-res))
              (tipar-cuerpos-letrec (cdr tipos-res) (cdr proc-names)
                                    (cdr idss) (cdr tipos-args)
                                    (cdr bodies) amb-rec)
              (eopl:error 'type-of
                          "El procedimiento ~s declara ~a y su cuerpo da ~a"
                          (car proc-names)
                          (tipo->texto (car tipos-res))
                          (tipo->texto tipo-cuerpo)))))))

;; tipar-acciones : lista de expresiones x tipo x tenv -> boolean
;; Verifica que todas las acciones de un case tengan el tipo dado.
;; Ejemplo: (tipar-acciones '() (num-tipo) (tenv-inicial)) => #t
(define tipar-acciones
  (lambda (acciones esperado amb)
    (if (null? acciones)
        #t
        (let ((t (type-of (car acciones) amb)))
          (if (tipos-iguales? t esperado)
              (tipar-acciones (cdr acciones) esperado amb)
              (eopl:error 'type-of
                          "Las ramas del case deben coincidir: ~a vs ~a"
                          (tipo->texto esperado) (tipo->texto t)))))))

;; tipar-secuencia : lista no vacía de expresiones x tenv -> tipo
;; Chequea cada expresión en orden y retorna el tipo de la última.
;; Ejemplo: (tipar-secuencia (list (lit-exp 1) (true-exp)) (tenv-inicial))
;;   => (bool-tipo)
(define tipar-secuencia
  (lambda (exps amb)
    (let ((t (type-of (car exps) amb)))
      (if (null? (cdr exps))
          t
          (tipar-secuencia (cdr exps) amb)))))

;; tipar-operador : tipo -> tipo
;; Retorna el tipo si es el de un procedimiento; si no, levanta
;; eopl:error «Se intento aplicar un valor de tipo <t>».
;; Ejemplo: (tipar-operador (proc-tipo '() (num-tipo))) => ese mismo tipo
(define tipar-operador
  (lambda (t)
    (cases tipo t
      (proc-tipo (args res) t)
      (else
       (eopl:error 'type-of
                   "Se intento aplicar un valor de tipo ~a"
                   (tipo->texto t))))))

;; type-of : expresion x tenv -> tipo
;; El tipo de la expresión bajo el ambiente de tipos, o eopl:error con
;; el mensaje que explique el conflicto. Una cláusula por variante, con la
;; regla del cuadro del enunciado.
;; Ejemplo: (type-of (lit-exp 5) (tenv-inicial)) => (num-tipo)
(define type-of
  (lambda (exp amb)
    (cases expresion exp
      (lit-exp (dato) (num-tipo))
      (var-exp (id) (apply-tenv amb id))
      (true-exp () (bool-tipo))
      (false-exp () (bool-tipo))
      (prim-exp (prim rands)
        (tipo-de-primitiva prim (tipar-rands rands amb)))
      (if-exp (test true-exp false-exp)
        (let ((t-test (type-of test amb)))
          (if (tipos-iguales? t-test (bool-tipo))
              (let ((t-then (type-of true-exp amb))
                    (t-else (type-of false-exp amb)))
                (if (tipos-iguales? t-then t-else)
                    t-then
                    (eopl:error 'type-of
                                "Las ramas del if deben coincidir: ~a vs ~a"
                                (tipo->texto t-then) (tipo->texto t-else))))
              (eopl:error 'type-of
                          "La condicion del if debe ser bool, se encontro ~a"
                          (tipo->texto t-test)))))
      (let-exp (ids rands body)
        (type-of body (tenv-extendido ids (tipar-rands rands amb) amb)))
      (proc-exp (ids tipos body)
        (proc-tipo tipos
                   (type-of body (tenv-extendido ids tipos amb))))
      (app-exp (rator rands)
        (let ((t-rator (tipar-operador (type-of rator amb)))
              (t-rands (tipar-rands rands amb)))
          (cases tipo t-rator
            (proc-tipo (args res)
              (begin (tipar-aridad args t-rands)
                     (tipar-argumentos args t-rands 1)
                     res))
            (else t-rator))))
      (app-ref-exp (rator ids)
        (let ((t-rator (tipar-operador (type-of rator amb)))
              (t-ids (tipar-rands (map var-exp ids) amb)))
          (cases tipo t-rator
            (proc-tipo (args res)
              (begin (tipar-aridad args t-ids)
                     (tipar-argumentos-ref args t-ids 1)
                     res))
            (else t-rator))))
      (letrec-exp (tipos-res proc-names idss tipos-args bodies letrec-body)
        (let ((amb-rec (tenv-recursivo proc-names tipos-args tipos-res amb)))
          (begin
            (tipar-cuerpos-letrec tipos-res proc-names idss tipos-args
                                  bodies amb-rec)
            (type-of letrec-body amb-rec))))
      (case-exp (control valores acciones default)
        (let ((t-control (type-of control amb)))
          (if (tipos-iguales? t-control (num-tipo))
              (let ((t-default (type-of default amb)))
                (begin (tipar-acciones acciones t-default amb)
                       t-default))
              (eopl:error 'type-of
                          "El valor de control del case debe ser num, se encontro ~a"
                          (tipo->texto t-control)))))
      (newref-exp (exp1)
        (ref-tipo (type-of exp1 amb)))
      (deref-exp (exp1)
        (let ((t1 (type-of exp1 amb)))
          (cases tipo t1
            (ref-tipo (s) s)
            (else
             (eopl:error 'type-of
                         "deref espera refto(t), se encontro ~a"
                         (tipo->texto t1))))))
      (setref-exp (ref-exp val-exp)
        (let ((t-ref (type-of ref-exp amb))
              (t-val (type-of val-exp amb)))
          (cases tipo t-ref
            (ref-tipo (s)
              (if (tipos-iguales? s t-val)
                  (num-tipo)
                  (eopl:error 'type-of
                              "La celda guarda ~a y se intento escribir ~a"
                              (tipo->texto s) (tipo->texto t-val))))
            (else
             (eopl:error 'type-of
                         "setref espera refto(t), se encontro ~a"
                         (tipo->texto t-ref))))))
      (swap-exp (ref1-exp ref2-exp)
        (let ((t1 (type-of ref1-exp amb))
              (t2 (type-of ref2-exp amb)))
          (cases tipo t1
            (ref-tipo (s1)
              (cases tipo t2
                (ref-tipo (s2)
                  (if (tipos-iguales? s1 s2)
                      s1
                      (eopl:error 'type-of
                                  "swap exige celdas del mismo tipo: ~a vs ~a"
                                  (tipo->texto t1) (tipo->texto t2))))
                (else
                 (eopl:error 'type-of
                             "swap espera refto(t), se encontro ~a"
                             (tipo->texto t2)))))
            (else
             (eopl:error 'type-of
                         "swap espera refto(t), se encontro ~a"
                         (tipo->texto t1))))))
      (begin-exp (exp1 exps)
        (tipar-secuencia (cons exp1 exps) amb)))))

;; type-of-program : programa -> tipo
;; Chequea el programa en el ambiente de tipos inicial.
;; Ejemplo: (type-of-program (a-program (lit-exp 1))) => (num-tipo)
(define type-of-program
  (lambda (pgm)
    (cases programa pgm
      (a-program (exp)
        (type-of exp (tenv-inicial))))))

;; tipo-de-texto : string -> string
;; Analiza el texto, lo chequea y describe el tipo resultante.
;; Ejemplo: (tipo-de-texto "newref(true)") => "refto(bool)"
(define tipo-de-texto
  (lambda (texto)
    (tipo->texto (type-of-program (parser texto)))))

(provide (all-defined-out))