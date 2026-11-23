#lang eopl
;Autores: Luis David Mendoza Manzano 2067621

#|
  Taller 3 — Cauca con referencias y tipos.
  Especificación léxica y gramatical.

  Aquí van los tokens, las producciones con las anotaciones de tipo, los
  datatypes que SLLGEN genera a partir de ellas (programa, expresion,
  tipo, primitiva), el scanner y el parser.

  El módulo no requiere ningún otro: lo requieren los cinco del
  intérprete y los dos del chequeador, de modo que la gramática se
  escribe una sola vez y los dos lados recorren el mismo árbol.

  La gramática es la del Taller 2 con los dos cambios del enunciado:
    1. seis expresiones nuevas: newref, deref, setref, swap, begin y
       applyref;
    2. anotaciones de tipo en proc y en letrec. El evaluador las ignora;
       solo las usa el chequeador.
|#

;;; ==========================================================
;;; 1. Especificación léxica
;;; ==========================================================
;; Los comentarios de línea, los identificadores y los números (enteros
;; y flotantes, con signo). El guion abre un número negativo, así que un
;; identificador no puede llevarlo.

(define especificacion-lexica
  '((white-sp (whitespace) skip)
    (comentario ("//" (arbno (not #\newline))) skip)
    (identificador (letter (arbno (or letter digit "_" "?"))) symbol)
    (numero (digit (arbno digit)) number)
    (numero ("-" digit (arbno digit)) number)
    (numero (digit (arbno digit) "." digit (arbno digit)) number)
    (numero ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;;; ==========================================================
;;; 2. Especificación gramatical
;;; ==========================================================
;; Cuatro no terminales: programa, expresion, tipo y primitiva.
;;
;; proc-exp(ids, tipos, body): los parámetros y sus tipos van en dos
;; listas paralelas, que es lo que SLLGEN produce con
;; (separated-list identificador ":" tipo ",").
;;
;; letrec-exp(tipos-res, proc-names, idss, tipos-args, bodies, body): el
;; tipo que antecede al nombre es el del resultado del procedimiento.
;; idss y tipos-args son listas de listas, una por procedimiento.
;;
;; begin-exp(exp, exps): la primera expresión y las que siguen a cada ";".
;; app-ref-exp(rator, ids): los argumentos de applyref son identificadores.

(define especificacion-gramatical
  '((programa (expresion) a-program)

    (expresion (numero) lit-exp)
    (expresion (identificador) var-exp)
    (expresion ("true") true-exp)
    (expresion ("false") false-exp)
    (expresion (primitiva "(" (separated-list expresion ",") ")") prim-exp)
    (expresion ("if" expresion "then" expresion "else" expresion "end")
               if-exp)
    (expresion ("let" (arbno identificador "=" expresion) "in" expresion "end")
               let-exp)
    (expresion ("proc" "(" (separated-list identificador ":" tipo ",") ")"
                expresion "end")
               proc-exp)
    (expresion ("apply" expresion "(" (separated-list expresion ",") ")")
               app-exp)
    (expresion ("letrec"
                (arbno tipo identificador
                       "(" (separated-list identificador ":" tipo ",") ")"
                       "=" expresion)
                "in" expresion "end")
               letrec-exp)
    (expresion ("case" expresion
                (arbno "of" numero "=>" expresion)
                "otherwise" "=>" expresion "end")
               case-exp)

    ;; Parte 1: el almacén
    (expresion ("newref" "(" expresion ")") newref-exp)
    (expresion ("deref" "(" expresion ")") deref-exp)
    (expresion ("setref" "(" expresion "," expresion ")") setref-exp)
    (expresion ("swap" "(" expresion "," expresion ")") swap-exp)
    (expresion ("begin" expresion (arbno ";" expresion) "end") begin-exp)

    ;; Parte 2: paso por referencia
    (expresion ("applyref" expresion "(" (separated-list identificador ",") ")")
               app-ref-exp)

    ;; Los tipos
    (tipo ("num") num-tipo)
    (tipo ("bool") bool-tipo)
    (tipo ("refto" "(" tipo ")") ref-tipo)
    (tipo ("(" (separated-list tipo "*") "->" tipo ")") proc-tipo)

    ;; Las primitivas del Taller 2
    (primitiva ("+") sum-prim)
    (primitiva ("-") minus-prim)
    (primitiva ("*") mult-prim)
    (primitiva ("/") div-prim)
    (primitiva ("add1") add-prim)
    (primitiva ("sub1") sub-prim)
    (primitiva (">") mayor-prim)
    (primitiva (">=") mayorigual-prim)
    (primitiva ("<") menor-prim)
    (primitiva ("<=") menorigual-prim)
    (primitiva ("==") igual-prim)
    (primitiva ("not") not-prim)
    (primitiva ("and") and-prim)
    (primitiva ("or") or-prim)
    (primitiva ("abs") abs-prim)
    (primitiva ("mcd") mcd-prim)
    (primitiva ("mcm") mcm-prim)
    (primitiva ("promedio") promedio-prim)
    (primitiva ("xor") xor-prim)
    (primitiva ("implica") implica-prim)
    (primitiva ("entre") entre-prim)))

;;; ==========================================================
;;; 3. Datatypes, scanner y parser
;;; ==========================================================

(sllgen:make-define-datatypes especificacion-lexica especificacion-gramatical)

;; mostrar-datatypes : -> void
;; Imprime los define-datatype que SLLGEN generó a partir de la gramática.
(define mostrar-datatypes
  (lambda () (sllgen:list-define-datatypes especificacion-lexica especificacion-gramatical)))

;; scanner : string -> lista de tokens
;; Convierte el texto de un programa en la lista de sus tokens; descarta
;; los espacios y los comentarios.
;; Ejemplo: (scanner "deref(c)") => tokens deref, "(", c y ")"
(define scanner
  (sllgen:make-string-scanner especificacion-lexica especificacion-gramatical))

;; parser : string -> programa
;; Convierte el texto de un programa en su árbol de sintaxis abstracta.
;; Ejemplo: (parser "newref(0)") => (a-program (newref-exp (lit-exp 0)))
(define parser
  (sllgen:make-string-parser especificacion-lexica especificacion-gramatical))

(provide (all-defined-out))