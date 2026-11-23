#lang eopl
;Autores: Luis David Mendoza Manzano 2067621

#|
  Taller 3 — Cauca con referencias y tipos.
  Evaluación y punto de entrada.

  evaluar-expresion recorre el árbol con cases y apply-procedure liga
  los parámetros de una clausura antes de evaluar su cuerpo. Las dos se
  llaman mutuamente, así que comparten módulo, y con ellas la copia por
  valor de los argumentos, que describe cómo entra un valor a apply y no
  cómo está hecha la clausura.

  El evaluador ignora las anotaciones de tipo: las lee el parser, quedan
  en el árbol y solo las usa el chequeador.

  Las cláusulas de newref-exp, deref-exp, setref-exp, swap-exp y
  begin-exp van en evaluar-expresion, junto a las demás variantes.
  Las tres que verifican con referencia? antes de tocar el almacén son
  deref-exp, setref-exp y swap-exp; si el valor no es una referencia,
  levantan eopl:error.

  Decisiones sobre lo que el enunciado no fija:
    - apply evalúa el operador, verifica que sea un procedimiento y luego
      evalúa los operandos de izquierda a derecha; a cada valor le aplica
      copia-por-valor antes de entregarlo al procedimiento.
    - applyref evalúa el operador, verifica que sea un procedimiento y
      busca cada identificador en el ambiente (sin copiar nada); la
      aridad la verifica apply-procedure, igual que en apply.
    - setref y swap evalúan sus dos operandos de izquierda a derecha y
      solo después verifican que sean referencias.
    - Una referencia es un valor como cualquier otro: puede guardarse en
      otra celda, pasarse a un procedimiento o devolverse de él. Las
      primitivas aritméticas, relacionales y booleanas la rechazan.

  Ejemplos (el resultado va después de =>; se comprueban en
  pruebas-cauca-refs.rkt):

    Parte 1 — almacén
    (interpretar "deref(newref(42))")                                   => 42
    (interpretar "setref(newref(1), 2)")                                => 1
    (interpretar "let c = newref(0) in begin setref(c, 5); deref(c) end end")
                                                                         => 5
    (interpretar "let a = newref(7) b = newref(2) in
                    begin swap(a, b); +(deref(a), *(10, deref(b))) end end")
                                                                         => 72
    (interpretar "let a = newref(7) b = newref(2) in swap(a, b) end")   => 2
    (interpretar "let c = newref(5) in let o = c in
                    begin setref(o, 99); deref(c) end end end")          => 99
    (interpretar "let r = newref(newref(3)) in deref(deref(r)) end")     => 3
    (interpretar "begin 1; 2; 3 end")                                   => 3
    (interpretar "let c = newref(1) in begin swap(c, c); deref(c) end end")
                                                                         => 1
    errores:
    (interpretar "deref(5)")              => error: deref exige una referencia
    (interpretar "setref(1, 2)")          => error: setref exige una referencia
    (interpretar "swap(1, newref(2))")    => error: swap exige referencias

    Parte 2 — paso por referencia
    applyref con intercambiar(x, y) sobre x = 11, y = 4 y -(deref(x), deref(y))
                                                                         => -7
    el mismo programa con apply en lugar de applyref                    => 7
    errores:
    applyref sobre una variable ligada a un número  => error
    applyref sobre un operador que no es procedimiento => error
|#

(require "sintaxis.rkt")
(require "almacen.rkt")
(require "ambiente.rkt")
(require "primitivas.rkt")

;; copia-por-valor : valor -> valor
;; El paso por valor entrega una copia del argumento. Para un número, un
;; booleano o un procedimiento la copia es el mismo valor; para una
;; referencia es una celda nueva con el mismo contenido. applyref no usa
;; esta función: ahí el parámetro queda ligado a la misma celda.
;; Ejemplo: (copia-por-valor 5) => 5
;;          sobre un almacén vacío, (copia-por-valor (newref! 7)) => (a-ref 1),
;;          y la celda 1 contiene 7
(define copia-por-valor
  (lambda (val)
    (if (referencia? val)
        (newref! (deref val))
        val)))

;; copiar-lista : lista de valores -> lista de valores
;; Aplica copia-por-valor a cada elemento, de izquierda a derecha.
;; Ejemplo: (copiar-lista '(1 #t)) => (1 #t)
(define copiar-lista
  (lambda (vals)
    (if (null? vals)
        '()
        (let ((primero (copia-por-valor (car vals))))
          (cons primero (copiar-lista (cdr vals)))))))

;; apply-procedure : procval x lista de valores -> valor
;; Verifica la aridad y evalúa el cuerpo en el ambiente de la clausura
;; extendido con los parámetros. No copia nada: lo que llegue en args es
;; lo que queda ligado (apply copia antes de llamar; applyref no).
;; Ejemplo: aplicar la clausura de proc (x : num) add1(x) end a (4) => 5
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (cerradura (ids body env)
        (if (= (length ids) (length args))
            (evaluar-expresion body (ambiente-extendido ids args env))
            (eopl:error 'apply-procedure
                        "El procedimiento espera ~s argumentos y recibio ~s"
                        (length ids) (length args)))))))

;; evaluar-rands : lista de expresiones x ambiente -> lista de valores
;; Evalúa las expresiones de izquierda a derecha en el mismo ambiente.
;; Ejemplo: (evaluar-rands (list (lit-exp 1) (var-exp 'n)) (ambiente-inicial))
;;   => (1 10)
(define evaluar-rands
  (lambda (rands env)
    (if (null? rands)
        '()
        (let ((primero (evaluar-expresion (car rands) env)))
          (cons primero (evaluar-rands (cdr rands) env))))))

;; evaluar-ids : lista de symbols x ambiente -> lista de valores
;; Busca cada identificador en el ambiente, de izquierda a derecha, y
;; verifica que su valor sea una referencia; si alguno no lo es, levanta
;; eopl:error. No copia nada: devuelve las mismas referencias.
;; Ejemplo: con x ligada a (a-ref 0), (evaluar-ids '(x) env) => ((a-ref 0))
(define evaluar-ids
  (lambda (ids env)
    (if (null? ids)
        '()
        (let ((valor (apply-env env (car ids))))
          (if (referencia? valor)
              (cons valor (evaluar-ids (cdr ids) env))
              (eopl:error 'applyref
                          "El paso por referencia exige variables ligadas a referencias, y ~s no lo esta"
                          (car ids)))))))

;; evaluar-secuencia : lista no vacía de expresiones x ambiente -> valor
;; Evalúa las expresiones de izquierda a derecha y retorna el valor de la
;; última. Es lo que hace observable el orden de evaluación.
;; Ejemplo: (evaluar-secuencia (list (lit-exp 1) (lit-exp 2)) (ambiente-inicial))
;;   => 2
(define evaluar-secuencia
  (lambda (exps env)
    (let ((valor (evaluar-expresion (car exps) env)))
      (if (null? (cdr exps))
          valor
          (evaluar-secuencia (cdr exps) env)))))

;; elegir-rama : valor x lista de números x lista de expresiones x
;;               expresion x ambiente -> valor
;; Recorre las ramas de un case en orden: evalúa la acción de la primera
;; cuyo literal es igual al valor de control, y si no hay ninguna evalúa
;; la rama otherwise. El valor de control ya viene evaluado.
(define elegir-rama
  (lambda (control valores acciones default env)
    (cond
      ((null? valores) (evaluar-expresion default env))
      ((= control (car valores)) (evaluar-expresion (car acciones) env))
      (else (elegir-rama control (cdr valores) (cdr acciones) default env)))))

;; exigir-referencia : valor x symbol -> referencia
;; Retorna el valor si es una referencia; si no, levanta eopl:error a
;; nombre de quien lo pidió.
;; Ejemplo: (exigir-referencia (a-ref 0) 'deref) => (a-ref 0)
(define exigir-referencia
  (lambda (val quien)
    (if (referencia? val)
        val
        (eopl:error quien "Se esperaba una referencia y llego ~s" val))))

;; evaluar-expresion : expresion x ambiente -> valor
;; Una cláusula por variante de la gramática. La condición de un if se
;; verifica con boolean? antes de ramificar; el case evalúa la expresión
;; de control una sola vez. Las anotaciones de tipo de proc y letrec se
;; ignoran.
;; Ejemplo: (evaluar-expresion (lit-exp 7) (ambiente-inicial)) => 7
(define evaluar-expresion
  (lambda (exp env)
    (cases expresion exp
      (lit-exp (dato) dato)
      (var-exp (id) (apply-env env id))
      (true-exp () #t)
      (false-exp () #f)
      (prim-exp (prim rands)
        (evaluar-primitiva prim (evaluar-rands rands env)))
      (if-exp (test true-exp false-exp)
        (let ((test-value (evaluar-expresion test env)))
          (if (boolean? test-value)
              (if test-value
                  (evaluar-expresion true-exp env)
                  (evaluar-expresion false-exp env))
              (eopl:error 'if-exp
                          "La condicion debe ser booleana y llego ~s"
                          test-value))))
      (let-exp (ids rands body)
        (evaluar-expresion body
                           (ambiente-extendido ids (evaluar-rands rands env) env)))
      (proc-exp (ids tipos body)
        (cerradura ids body env))
      (app-exp (rator rands)
        (let ((proc (evaluar-expresion rator env)))
          (if (procval? proc)
              (apply-procedure proc (copiar-lista (evaluar-rands rands env)))
              (eopl:error 'app-exp
                          "Se intento aplicar algo que no es un procedimiento: ~s"
                          proc))))
      (letrec-exp (tipos-res proc-names idss tipos-args bodies letrec-body)
        (evaluar-expresion letrec-body
                           (ambiente-recursivo proc-names idss bodies env)))
      (case-exp (control valores acciones default)
        (let ((control-value (evaluar-expresion control env)))
          (if (number? control-value)
              (elegir-rama control-value valores acciones default env)
              (eopl:error 'case-exp
                          "El valor de control debe ser un numero y llego ~s"
                          control-value))))
      (newref-exp (exp1)
        (newref! (evaluar-expresion exp1 env)))
      (deref-exp (exp1)
        (let ((ref (evaluar-expresion exp1 env)))
          (if (referencia? ref)
              (deref ref)
              (eopl:error 'deref-exp
                          "deref exige una referencia y llego ~s"
                          ref))))
      (setref-exp (ref-exp val-exp)
        (let* ((ref (evaluar-expresion ref-exp env))
               (val (evaluar-expresion val-exp env)))
          (if (referencia? ref)
              (begin (setref! ref val)
                     1)
              (eopl:error 'setref-exp
                          "setref exige una referencia y llego ~s"
                          ref))))
      (swap-exp (ref1-exp ref2-exp)
        (let* ((ref1 (evaluar-expresion ref1-exp env))
               (ref2 (evaluar-expresion ref2-exp env)))
          (if (and (referencia? ref1) (referencia? ref2))
              (let ((contenido1 (deref ref1))
                    (contenido2 (deref ref2)))
                (begin (setref! ref1 contenido2)
                       (setref! ref2 contenido1)
                       contenido2))
              (eopl:error 'swap-exp
                          "swap exige dos referencias y llegaron ~s y ~s"
                          ref1 ref2))))
      (begin-exp (exp1 exps)
        (evaluar-secuencia (cons exp1 exps) env))
      (app-ref-exp (rator ids)
        (let ((proc (evaluar-expresion rator env)))
          (if (procval? proc)
              (apply-procedure proc (evaluar-ids ids env))
              (eopl:error 'app-ref-exp
                          "Se intento aplicar algo que no es un procedimiento: ~s"
                          proc)))))))

;; evaluar-programa : programa -> valor
;; Deja el almacén vacío y evalúa el cuerpo en el ambiente inicial.
;; Ejemplo: (evaluar-programa (a-program (var-exp 'n))) => 10
(define evaluar-programa
  (lambda (pgm)
    (begin
      (initialize-store!)
      (cases programa pgm
        (a-program (exp)
          (evaluar-expresion exp (ambiente-inicial)))))))

;; interpretar : string -> valor
;; Analiza el texto con el parser y lo evalúa.
;; Ejemplo: (interpretar "+(n, m)") => 13
(define interpretar
  (lambda (texto)
    (evaluar-programa (parser texto))))

;; interpretador : -> void
;; El ciclo de lectura, con sllgen:make-rep-loop sobre evaluar-programa.
;; make-rep-loop devuelve el ciclo, que se invoca aquí; lee un programa
;; por línea y escribe su valor.
(define interpretador
  (lambda ()
    ((sllgen:make-rep-loop
      "--> "
      evaluar-programa
      (sllgen:make-stream-parser especificacion-lexica especificacion-gramatical)))))

(provide (all-defined-out))