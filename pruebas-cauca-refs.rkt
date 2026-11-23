#lang eopl
;Autores: Luis David Mendoza Manzano 2067621

#|
  Pruebas del interpretador con referencias y del chequeador de tipos.

  Se ejecuta con:  racket pruebas-cauca-refs.rkt
|#

(require rackunit)
(require rackunit/text-ui)
(require (only-in racket/base exn:fail? with-handlers exn-message))
(require "interprete-cauca-refs.rkt")
(require "checker-cauca.rkt")

(define corre (lambda (texto) (interpretar texto)))
(define tipo-de (lambda (texto) (tipo-de-texto texto)))

;; mensaje-de : string -> string
;; El mensaje que produce el chequeador, para comprobarlo textualmente.
(define mensaje-de
  (lambda (texto)
    (with-handlers ((exn:fail? (lambda (e) (exn-message e))))
      (string-append "sin error: " (tipo-de texto)))))

;; contiene? : string x string -> boolean
(define contiene?
  (lambda (texto fragmento)
    (letrec ((desde (lambda (i)
                      (cond ((> (+ i (string-length fragmento)) (string-length texto)) #f)
                            ((string=? (substring texto i (+ i (string-length fragmento)))
                                       fragmento) #t)
                            (else (desde (+ i 1)))))))
      (desde 0))))

;;; ==========================================================
;;; Interprete
;;; ==========================================================

(define almacen
  (test-suite
   "Parte 1: almacen y expresiones nuevas"
   (test-case "newref y deref"
              (check-equal? (corre "deref(newref(42))") 42)
              (check-equal? (corre "deref(newref(true))") #t))
   (test-case "setref escribe y retorna 1"
              (check-equal? (corre "setref(newref(1), 2)") 1)
              (check-equal? (corre "let c = newref(1) in begin setref(c, 9); deref(c) end end") 9))
   (test-case "el contador sobrevive entre llamadas"
              (check-equal? (corre "let contador = newref(0)
in
  begin
    setref(contador, add1(deref(contador)));
    setref(contador, add1(deref(contador)));
    setref(contador, add1(deref(contador)));
    deref(contador)
  end
end") 3))
   (test-case "swap intercambia el contenido y retorna el nuevo valor de la primera"
              (check-equal? (corre "let a = newref(7)
    b = newref(2)
in
  begin
    swap(a, b);
    +(deref(a), *(10, deref(b)))
  end
end") 72)
              (check-equal? (corre "let a = newref(7) b = newref(2) in swap(a, b) end") 2))
   (test-case "begin evalua de izquierda a derecha y retorna la ultima"
              (check-equal? (corre "begin 1; 2; 3 end") 3)
              (check-equal? (corre "let c = newref(0)
                                    in begin setref(c, 5); setref(c, 6); deref(c) end end") 6))
   (test-case "alias: dos nombres, una sola celda"
              (check-equal? (corre "let celda = newref(5)
in
  let otro = celda
  in
    begin
      setref(otro, 99);
      deref(celda)
    end
  end
end") 99))
   (test-case "el almacen arranca vacio en cada programa"
              (check-equal? (corre "deref(newref(1))") 1)
              (check-equal? (corre "deref(newref(1))") 1))
   (test-case "los casos de error del almacen"
              (check-exn exn:fail? (lambda () (corre "deref(5)")))
              (check-exn exn:fail? (lambda () (corre "setref(1, 2)")))
              (check-exn exn:fail? (lambda () (corre "swap(1, newref(2))"))))))

(define paso-por-referencia
  (test-suite
   "Parte 2: paso por referencia"
   (test-case "applyref deja que el procedimiento escriba en la celda del que llama"
              (check-equal? (corre "let x = newref(11)
    y = newref(4)
in
  let intercambiar = proc (u : refto(num), v : refto(num))
                       swap(u, v)
                     end
  in
    begin
      applyref intercambiar (x, y);
      -(deref(x), deref(y))
    end
  end
end") -7))
   (test-case "el mismo programa con apply no toca las celdas del que llama"
              (check-equal? (corre "let x = newref(11)
    y = newref(4)
in
  let intercambiar = proc (u : refto(num), v : refto(num))
                       swap(u, v)
                     end
  in
    begin
      apply intercambiar (x, y);
      -(deref(x), deref(y))
    end
  end
end") 7))
   (test-case "applyref sobre una sola celda"
              (check-equal? (corre "let c = newref(1)
                                    in let poner = proc (u : refto(num)) setref(u, 100) end
                                       in begin applyref poner (c); deref(c) end end end") 100))
   (test-case "applyref con dos celdas independientes"
              (check-equal? (corre "let a = newref(1)
    b = newref(2)
in let sumar = proc (u : refto(num), v : refto(num))
                 setref(u, +(deref(u), deref(v)))
               end
   in begin applyref sumar (a, b); *(deref(a), deref(b)) end
end end") 6))
   (test-case "apply si comparte el procedimiento, no la celda"
              (check-equal? (corre "let c = newref(1)
                                    in let poner = proc (u : refto(num)) setref(u, 100) end
                                       in begin apply poner (c); deref(c) end end end") 1))
   (test-case "applyref exige variables ligadas a referencias"
              (check-exn exn:fail?
                         (lambda () (corre "let z = 1
                                            in let f = proc (u : refto(num)) deref(u) end
                                               in applyref f (z) end end"))))
   (test-case "applyref exige un procedimiento como operador"
              (check-exn exn:fail?
                         (lambda () (corre "let z = newref(1) in applyref 5 (z) end"))))))

;;; ==========================================================
;;; Chequeador
;;; ==========================================================

(define bien-tipados
  (test-suite
   "Parte 3: programas bien tipados"
   (test-case "procedimiento anotado"
              (check-equal? (tipo-de "proc (a : num, b : num) +(a, b) end") "(num * num -> num)"))
   (test-case "celda con lectura y escritura"
              (check-equal? (tipo-de "let celda = newref(0)
in
  begin
    setref(celda, 42);
    deref(celda)
  end
end") "num"))
   (test-case "newref anida los tipos"
              (check-equal? (tipo-de "newref(newref(true))") "refto(refto(bool))"))
   (test-case "swap devuelve el tipo de la celda"
              (check-equal? (tipo-de "let a = newref(1) b = newref(2) in swap(a, b) end") "num"))
   (test-case "letrec recursivo"
              (check-equal? (tipo-de "letrec num fact (k : num) =
                                        if ==(k, 0) then 1 else *(k, apply fact (sub1(k))) end
                                      in apply fact (5) end") "num"))
   (test-case "applyref con una celda"
              (check-equal? (tipo-de "let x = newref(1)
                                      in let f = proc (u : refto(num)) deref(u) end
                                         in applyref f (x) end end") "num"))
   (test-case "case con ramas del mismo tipo"
              (check-equal? (tipo-de "case n of 1 => 10 of 2 => 20 otherwise => 0 end") "num"))
   (test-case "procedimiento de orden superior"
              (check-equal? (tipo-de "proc (f : (num -> num), x : num) apply f (x) end")
                            "((num -> num) * num -> num)"))))

(define mal-tipados
  (test-suite
   "Parte 3: cada mensaje de error"
   (test-case "la condicion del if no es booleana"
              (check-true (contiene? (mensaje-de "if n then 1 else 2 end")
                                     "La condicion del if debe ser bool, se encontro num")))
   (test-case "las ramas del if no coinciden"
              (check-true (contiene? (mensaje-de "if >(n, m) then 1 else false end")
                                     "Las ramas del if deben coincidir: num vs bool")))
   (test-case "se aplica algo que no es procedimiento"
              (check-true (contiene? (mensaje-de "apply 5 (1)")
                                     "Se intento aplicar un valor de tipo num")))
   (test-case "aridad incorrecta"
              (check-true (contiene? (mensaje-de "let f = proc (a : num, b : num) +(a, b) end
                                                  in apply f (1) end")
                                     "El procedimiento espera 2 argumentos y recibio 1")))
   (test-case "argumento de tipo equivocado"
              (check-true (contiene? (mensaje-de "let f = proc (a : num) a end
                                                  in apply f (true) end")
                                     "El argumento 1 debe ser num, se encontro bool")))
   (test-case "deref sobre algo que no es referencia"
              (check-true (contiene? (mensaje-de "deref(5)")
                                     "deref espera refto(t), se encontro num")))
   (test-case "setref con tipos incompatibles"
              (check-true (contiene? (mensaje-de "let celda = newref(0)
                                                  in setref(celda, true) end")
                                     "La celda guarda num y se intento escribir bool")))
   (test-case "el cuerpo del letrec no da el tipo declarado"
              (check-true (contiene? (mensaje-de "letrec num f (k : num) = true
                                                  in apply f (1) end")
                                     "El procedimiento f declara num y su cuerpo da bool")))
   (test-case "identificador no declarado"
              (check-true (contiene? (mensaje-de "z") "Variable no declarada: z")))
   (test-case "applyref con una variable que no es referencia"
              (check-exn exn:fail?
                         (lambda () (tipo-de "let z = 1
                                              in let f = proc (u : refto(num)) deref(u) end
                                                 in applyref f (z) end end"))))))

(define fallidas
  (+ (run-tests almacen)
     (run-tests paso-por-referencia)
     (run-tests bien-tipados)
     (run-tests mal-tipados)))

(eopl:printf "~%==========================================~%")
(eopl:printf "  pruebas fallidas en total: ~s~%" fallidas)
(eopl:printf "==========================================~%")

;;; ==========================================================
;;; Pruebas del grupo (se agregan a continuacion de las del fork)
;;; ==========================================================

;; fallo-con? : thunk x string -> boolean
;; #t si evaluar el thunk levanta un error cuyo mensaje contiene el
;; fragmento; #f si no hay error o el mensaje es otro.
(define fallo-con?
  (lambda (thunk fragmento)
    (contiene? (with-handlers ((exn:fail? (lambda (e) (exn-message e))))
                 (begin (thunk) "sin error"))
               fragmento)))

;; --- El almacen, operacion por operacion ---------------------------

(define almacen-tad
  (test-suite
   "Grupo - Parte 1: el TAD referencia y las cuatro operaciones"
   (test-case "initialize-store! deja el almacen vacio"
              (begin (initialize-store!)
                     (check-equal? (tamano-almacen) 0)
                     (check-equal? (contenido-almacen) '())
                     (check-equal? (empty-store) '())))
   (test-case "newref! agrega al final, retorna una referencia y no toca las demas"
              (begin (initialize-store!)
                     (let ((r0 (newref! 10))
                           (r1 (newref! 20)))
                       (begin
                         (check-true (referencia? r0))
                         (check-true (referencia? r1))
                         (check-equal? (contenido-almacen) '(10 20))
                         (check-equal? (deref r0) 10)
                         (check-equal? (deref r1) 20)))))
   (test-case "una referencia no es un numero"
              (begin (check-false (referencia? 0))
                     (check-false (referencia? #t))
                     (check-true (referencia? (a-ref 0)))))
   (test-case "setref! cambia exactamente una celda y el tamano no varia"
              (begin (initialize-store!)
                     (let ((r0 (newref! 1)) (r1 (newref! 2)) (r2 (newref! 3)))
                       (begin
                         (setref! r1 99)
                         (check-equal? (contenido-almacen) '(1 99 3))
                         (check-equal? (tamano-almacen) 3)
                         (setref! r0 7)
                         (setref! r2 8)
                         (check-equal? (contenido-almacen) '(7 99 8))))))
   (test-case "deref no modifica el almacen"
              (begin (initialize-store!)
                     (let ((r (newref! 5)))
                       (begin (deref r) (deref r)
                              (check-equal? (contenido-almacen) '(5))))))
   (test-case "el almacen guarda cualquier valor, tambien otras referencias"
              (begin (initialize-store!)
                     (let* ((r0 (newref! #t))
                            (r1 (newref! r0)))
                       (begin
                         (check-equal? (deref r0) #t)
                         (check-true (referencia? (deref r1)))
                         (check-equal? (deref (deref r1)) #t)))))
   (test-case "deref y setref! fuera de rango levantan eopl:error"
              (begin (initialize-store!)
                     (newref! 1)
                     (check-true (fallo-con? (lambda () (deref (a-ref 5))) "fuera de rango"))
                     (check-true (fallo-con? (lambda () (deref (a-ref -1))) "fuera de rango"))
                     (check-true (fallo-con? (lambda () (setref! (a-ref 1) 0)) "fuera de rango"))
                     (check-equal? (contenido-almacen) '(1))))))

;; --- Las expresiones nuevas desde el programa ----------------------

(define expresiones-nuevas
  (test-suite
   "Grupo - Parte 1: las cinco expresiones nuevas"
   (test-case "newref retorna una referencia y reserva una celda"
              (begin (check-equal? (corre "deref(newref(+(2, 3)))") 5)
                     (corre "newref(8)")
                     (check-equal? (tamano-almacen) 1)))
   (test-case "setref retorna 1 y la celda queda con el valor escrito"
              (check-equal? (corre "setref(newref(0), 5)") 1)
              (check-equal? (corre "let c = newref(0) in begin setref(c, 5); deref(c) end end") 5)
              (check-equal? (corre "let c = newref(0) in begin setref(c, true); deref(c) end end") #t))
   (test-case "swap retorna el contenido anterior de la segunda celda"
              (check-equal? (corre "let a = newref(7) b = newref(2) in swap(a, b) end") 2)
              (check-equal? (corre "let a = newref(7) b = newref(2) in begin swap(a, b); deref(b) end end") 7))
   (test-case "swap no pierde ningun valor y no toca las celdas ajenas"
              (check-equal? (corre "let a = newref(1) b = newref(2) c = newref(3)
                                    in begin swap(a, c); +(deref(a), *(10, deref(b)), *(100, deref(c))) end end")
                            123))
   (test-case "swap de una referencia consigo misma deja la celda igual"
              (check-equal? (corre "let c = newref(4) in begin swap(c, c); deref(c) end end") 4)
              (check-equal? (corre "let c = newref(4) in swap(c, c) end") 4))
   (test-case "swap dos veces restaura el estado"
              (check-equal? (corre "let a = newref(1) b = newref(2)
                                    in begin swap(a, b); swap(a, b); -(deref(a), deref(b)) end end")
                            -1))
   (test-case "begin retorna el valor de la ultima y respeta el orden"
              (check-equal? (corre "begin 1 end") 1)
              (check-equal? (corre "let c = newref(1)
                                    in begin setref(c, 2); setref(c, *(deref(c), 10)); deref(c) end end") 20)
              (check-equal? (corre "let c = newref(1)
                                    in begin setref(c, *(deref(c), 10)); setref(c, 2); deref(c) end end") 2))
   (test-case "los operandos se evaluan de izquierda a derecha"
              (check-equal? (corre "let c = newref(1)
                                    in -(deref(c), begin setref(c, 5); 0 end) end") 1)
              (check-equal? (corre "let c = newref(1)
                                    in -(begin setref(c, 5); 0 end, deref(c)) end") -5))
   (test-case "alias: la ligadura con let copia la referencia, no la celda"
              (check-equal? (corre "let c = newref(1) in let d = c in begin setref(d, 50); deref(c) end end end") 50)
              (check-equal? (corre "let c = newref(1) in let d = newref(deref(c))
                                    in begin setref(d, 50); deref(c) end end end") 1))
   (test-case "una referencia es un valor de primera clase"
              (check-equal? (corre "let r = newref(newref(3)) in deref(deref(r)) end") 3)
              (check-equal? (corre "let r = newref(newref(3))
                                    in begin setref(deref(r), 9); deref(deref(r)) end end") 9)
              (check-equal? (corre "let f = proc (x : num) newref(x) end in deref(apply f (6)) end") 6))
   (test-case "un procedimiento guardado en una celda se puede recuperar y aplicar"
              (check-equal? (corre "let c = newref(proc (x : num) add1(x) end)
                                    in apply deref(c) (4) end") 5))
   (test-case "el almacen empieza vacio en cada programa"
              (begin (corre "let a = newref(1) b = newref(2) c = newref(3) in 0 end")
                     (check-equal? (tamano-almacen) 3)
                     (corre "newref(0)")
                     (check-equal? (tamano-almacen) 1)))))

(define errores-de-referencias
  (test-suite
   "Grupo - Parte 1: errores del almacen"
   (test-case "deref exige una referencia"
              (check-true (fallo-con? (lambda () (corre "deref(5)")) "deref exige una referencia"))
              (check-true (fallo-con? (lambda () (corre "deref(true)")) "deref exige una referencia")))
   (test-case "setref exige una referencia en la primera posicion"
              (check-true (fallo-con? (lambda () (corre "setref(1, 2)")) "setref exige una referencia")))
   (test-case "swap exige dos referencias"
              (check-true (fallo-con? (lambda () (corre "swap(1, newref(2))")) "swap exige dos referencias"))
              (check-true (fallo-con? (lambda () (corre "swap(newref(2), 1)")) "swap exige dos referencias")))
   (test-case "una referencia no sirve de operando de una primitiva"
              (check-true (fallo-con? (lambda () (corre "+(newref(1), 2)")) "Se esperaba un numero"))
              (check-true (fallo-con? (lambda () (corre "if newref(true) then 1 else 2 end")) "booleana")))
   (test-case "una variable no ligada sigue siendo un error"
              (check-true (fallo-con? (lambda () (corre "deref(zzz)")) "no esta ligada")))))

;; --- Paso por valor y por referencia ------------------------------

(define valor-y-referencia
  (test-suite
   "Grupo - Parte 2: apply copia, applyref comparte"
   (test-case "apply copia la celda: aparece una celda nueva en el almacen"
              (begin (corre "let c = newref(1) in let f = proc (u : refto(num)) 0 end in apply f (c) end end")
                     (check-equal? (tamano-almacen) 2)))
   (test-case "applyref no copia nada: el almacen no crece"
              (begin (corre "let c = newref(1) in let f = proc (u : refto(num)) 0 end in applyref f (c) end end")
                     (check-equal? (tamano-almacen) 1)))
   (test-case "con apply el procedimiento escribe en su copia, con applyref en la celda"
              (check-equal? (corre "let c = newref(1) in let f = proc (u : refto(num)) setref(u, 7) end
                                    in begin apply f (c); deref(c) end end end") 1)
              (check-equal? (corre "let c = newref(1) in let f = proc (u : refto(num)) setref(u, 7) end
                                    in begin applyref f (c); deref(c) end end end") 7))
   (test-case "la copia conserva el contenido que tenia la celda"
              (check-equal? (corre "let c = newref(41) in let f = proc (u : refto(num)) add1(deref(u)) end
                                    in apply f (c) end end") 42))
   (test-case "para un numero, apply y la copia son indistinguibles"
              (check-equal? (corre "let f = proc (x : num) *(x, x) end in apply f (6) end") 36))
   (test-case "applyref con un incremento repetido sobre la misma celda"
              (check-equal? (corre "let c = newref(0)
                                    in let inc = proc (u : refto(num)) setref(u, add1(deref(u))) end
                                       in begin applyref inc (c); applyref inc (c); applyref inc (c); deref(c) end
                                    end end") 3))
   (test-case "applyref sobre la misma celda dos veces: los dos parametros son alias"
              (check-equal? (corre "let c = newref(5)
                                    in let f = proc (u : refto(num), v : refto(num))
                                                 begin setref(u, 1); deref(v) end
                                               end
                                       in applyref f (c, c) end end") 1)
              (check-equal? (corre "let c = newref(5)
                                    in let f = proc (u : refto(num), v : refto(num))
                                                 begin setref(u, 1); deref(v) end
                                               end
                                       in apply f (c, c) end end") 5))
   (test-case "applyref con tres celdas: rotar a, b, c"
              (check-equal? (corre "let a = newref(1) b = newref(2) c = newref(3)
                                    in let rotar = proc (x : refto(num), y : refto(num), z : refto(num))
                                                     begin swap(x, y); swap(y, z) end
                                                   end
                                       in begin applyref rotar (a, b, c);
                                                +(deref(a), *(10, deref(b)), *(100, deref(c))) end
                                    end end") 132))
   (test-case "la variable de applyref puede ser un parametro que ya es referencia"
              (check-equal? (corre "let c = newref(2)
                                    in let doble = proc (u : refto(num)) setref(u, *(2, deref(u))) end
                                       in let dos_veces = proc (w : refto(num))
                                                            begin applyref doble (w); applyref doble (w) end
                                                          end
                                          in begin applyref dos_veces (c); deref(c) end
                                       end end end") 8))
   (test-case "una recursion con apply copia la celda en cada llamada"
              (check-equal? (corre "letrec num suma (k : num, acc : refto(num)) =
                                      if ==(k, 0)
                                      then deref(acc)
                                      else begin setref(acc, +(deref(acc), k)); apply suma (sub1(k), acc) end
                                      end
                                    in let a = newref(0) in begin apply suma (4, a); deref(a) end end end") 0))))

(define errores-de-applyref
  (test-suite
   "Grupo - Parte 2: errores de applyref"
   (test-case "exige que el argumento este ligado a una referencia"
              (check-true (fallo-con? (lambda () (corre "let z = 1 in let f = proc (u : refto(num)) deref(u) end
                                                         in applyref f (z) end end"))
                                      "exige variables ligadas a referencias"))
              (check-true (fallo-con? (lambda () (corre "let f = proc (u : refto(num)) 0 end in applyref f (n) end"))
                                      "n no lo esta")))
   (test-case "exige que el operador sea un procedimiento"
              (check-true (fallo-con? (lambda () (corre "let z = newref(1) in applyref 5 (z) end"))
                                      "no es un procedimiento")))
   (test-case "exige la aridad correcta"
              (check-true (fallo-con? (lambda () (corre "let z = newref(1)
                                                         in let f = proc (a : refto(num), b : refto(num)) 0 end
                                                            in applyref f (z) end end"))
                                      "espera 2 argumentos y recibio 1")))
   (test-case "un identificador que no esta ligado es un error"
              (check-true (fallo-con? (lambda () (corre "let f = proc (u : refto(num)) 0 end in applyref f (zzz) end"))
                                      "no esta ligada")))
   (test-case "apply tambien exige procedimiento y aridad"
              (check-true (fallo-con? (lambda () (corre "apply 5 (1)")) "no es un procedimiento"))
              (check-true (fallo-con? (lambda () (corre "let f = proc (a : num) a end in apply f (1, 2) end"))
                                      "espera 1 argumentos y recibio 2")))))

;; --- El interprete de Taller 2 sigue funcionando -------------------

(define sin-regresion
  (test-suite
   "Grupo: lo del Taller 2 se conserva"
   (test-case "primitivas"
              (check-equal? (corre "+(n, m)") 13)
              (check-equal? (corre "-(10, 2, 3)") 5)
              (check-equal? (corre "promedio(1, 2, 3, 4)") 5/2)
              (check-equal? (corre "mcd(12, 18)") 6)
              (check-equal? (corre "entre(5, 1, 10)") #t))
   (test-case "if, let, proc, apply, letrec y case"
              (check-equal? (corre "if >(n, m) then 1 else 2 end") 1)
              (check-equal? (corre "let a = 2 b = 3 in *(a, b) end") 6)
              (check-equal? (corre "apply proc (x : num, y : num) -(x, y) end (9, 4)") 5)
              (check-equal? (corre "letrec num potencia (b : num, e : num) =
                                      if ==(e, 0) then 1 else *(b, apply potencia (b, sub1(e))) end
                                    in apply potencia (2, 10) end") 1024)
              (check-equal? (corre "case 2 of 1 => 10 of 2 => 20 otherwise => 0 end") 20))
   (test-case "alcance estatico"
              (check-equal? (corre "let factor = 10
                                    in let escalar = proc (v : num) *(v, factor) end
                                       in let factor = 1000 in apply escalar (5) end end end") 50))
   (test-case "errores de siempre"
              (check-true (fallo-con? (lambda () (corre "if 1 then 2 else 3 end")) "booleana"))
              (check-true (fallo-con? (lambda () (corre "/(1, 0)")) "Division por cero")))
   (test-case "los comentarios se ignoran"
              (check-equal? (corre "// una linea
+(1, 2) // otra") 3))
   (test-case "una anotacion mal escrita es un error del parser"
              (check-exn exn:fail? (lambda () (corre "proc (a) a end")))
              (check-exn exn:fail? (lambda () (corre "setref(c)"))))))

;; --- El chequeador ---------------------------------------------------

(define tipos-y-ambiente
  (test-suite
   "Grupo - Parte 3: tipos y ambiente de tipos"
   (test-case "tipo->texto"
              (check-equal? (tipo->texto (num-tipo)) "num")
              (check-equal? (tipo->texto (bool-tipo)) "bool")
              (check-equal? (tipo->texto (ref-tipo (ref-tipo (num-tipo)))) "refto(refto(num))")
              (check-equal? (tipo->texto (proc-tipo (list (num-tipo) (bool-tipo)) (num-tipo)))
                            "(num * bool -> num)")
              (check-equal? (tipo->texto (proc-tipo '() (num-tipo))) "(-> num)"))
   (test-case "tipos-iguales? compara por estructura"
              (check-true (tipos-iguales? (num-tipo) (num-tipo)))
              (check-false (tipos-iguales? (num-tipo) (bool-tipo)))
              (check-true (tipos-iguales? (ref-tipo (num-tipo)) (ref-tipo (num-tipo))))
              (check-false (tipos-iguales? (ref-tipo (num-tipo)) (ref-tipo (bool-tipo))))
              (check-false (tipos-iguales? (ref-tipo (num-tipo)) (num-tipo)))
              (check-true (tipos-iguales? (proc-tipo (list (num-tipo)) (bool-tipo))
                                          (proc-tipo (list (num-tipo)) (bool-tipo))))
              (check-false (tipos-iguales? (proc-tipo (list (num-tipo)) (bool-tipo))
                                           (proc-tipo (list (num-tipo) (num-tipo)) (bool-tipo))))
              (check-false (tipos-iguales? (proc-tipo (list (num-tipo)) (bool-tipo))
                                           (proc-tipo (list (num-tipo)) (num-tipo)))))
   (test-case "el ambiente de tipos inicial declara n, m, p, q y r como num"
              (check-true (tipos-iguales? (apply-tenv (tenv-inicial) 'n) (num-tipo)))
              (check-true (tipos-iguales? (apply-tenv (tenv-inicial) 'r) (num-tipo)))
              (check-true (fallo-con? (lambda () (apply-tenv (tenv-inicial) 'z))
                                      "Variable no declarada: z")))
   (test-case "tenv-extendido respeta el shadowing y deja ver lo de afuera"
              (let ((amb (tenv-extendido '(n x) (list (bool-tipo) (ref-tipo (num-tipo))) (tenv-inicial))))
                (begin
                  (check-true (tipos-iguales? (apply-tenv amb 'n) (bool-tipo)))
                  (check-true (tipos-iguales? (apply-tenv amb 'x) (ref-tipo (num-tipo))))
                  (check-true (tipos-iguales? (apply-tenv amb 'm) (num-tipo))))))
   (test-case "tenv-recursivo arma el tipo del procedimiento al buscarlo"
              (let ((amb (tenv-recursivo '(f g)
                                         (list (list (num-tipo)) (list (bool-tipo) (num-tipo)))
                                         (list (bool-tipo) (num-tipo))
                                         (tenv-vacio))))
                (begin
                  (check-true (tipos-iguales? (apply-tenv amb 'f)
                                              (proc-tipo (list (num-tipo)) (bool-tipo))))
                  (check-true (tipos-iguales? (apply-tenv amb 'g)
                                              (proc-tipo (list (bool-tipo) (num-tipo)) (num-tipo))))
                  (check-true (fallo-con? (lambda () (apply-tenv amb 'h)) "Variable no declarada: h")))))
   (test-case "tipo-de-primitiva"
              (check-true (tipos-iguales? (tipo-de-primitiva (sum-prim) (list (num-tipo) (num-tipo) (num-tipo)))
                                          (num-tipo)))
              (check-true (tipos-iguales? (tipo-de-primitiva (menor-prim) (list (num-tipo) (num-tipo)))
                                          (bool-tipo)))
              (check-true (tipos-iguales? (tipo-de-primitiva (and-prim) (list (bool-tipo) (bool-tipo)))
                                          (bool-tipo)))
              (check-true (tipos-iguales? (tipo-de-primitiva (entre-prim) (list (num-tipo) (num-tipo) (num-tipo)))
                                          (bool-tipo)))
              (check-true (fallo-con? (lambda () (tipo-de-primitiva (and-prim) (list (num-tipo) (bool-tipo))))
                                      "El argumento 1 de la primitiva and debe ser bool, se encontro num"))
              (check-true (fallo-con? (lambda () (tipo-de-primitiva (add-prim) (list (num-tipo) (num-tipo))))
                                      "espera 1 argumentos y recibio 2")))))

(define aciertos-del-chequeador
  (test-suite
   "Grupo - Parte 3: mas programas bien tipados"
   (test-case "literales, booleanos e identificadores"
              (check-equal? (tipo-de "5") "num")
              (check-equal? (tipo-de "-3.5") "num")
              (check-equal? (tipo-de "true") "bool")
              (check-equal? (tipo-de "q") "num"))
   (test-case "primitivas aritmeticas, relacionales y booleanas"
              (check-equal? (tipo-de "+(1, 2, 3)") "num")
              (check-equal? (tipo-de "promedio(1, 2)") "num")
              (check-equal? (tipo-de ">=(n, m)") "bool")
              (check-equal? (tipo-de "and(true, <(1, 2))") "bool")
              (check-equal? (tipo-de "entre(n, 0, 20)") "bool")
              (check-equal? (tipo-de "implica(true, false)") "bool"))
   (test-case "if con ramas del mismo tipo, aunque no sea num"
              (check-equal? (tipo-de "if >(n, m) then 1 else 2 end") "num")
              (check-equal? (tipo-de "if true then newref(1) else newref(2) end") "refto(num)"))
   (test-case "let liga identificadores con el tipo de su expresion"
              (check-equal? (tipo-de "let a = 1 b = true in if b then a else 0 end end") "num")
              (check-equal? (tipo-de "let n = true in n end") "bool"))
   (test-case "proc y apply"
              (check-equal? (tipo-de "proc (a : num, b : num) +(a, b) end") "(num * num -> num)")
              (check-equal? (tipo-de "proc () 5 end") "(-> num)")
              (check-equal? (tipo-de "apply proc (x : num) add1(x) end (3)") "num")
              (check-equal? (tipo-de "proc (u : refto(num)) deref(u) end") "(refto(num) -> num)"))
   (test-case "los cuatro tipos de referencias"
              (check-equal? (tipo-de "newref(7)") "refto(num)")
              (check-equal? (tipo-de "deref(newref(7))") "num")
              (check-equal? (tipo-de "let c = newref(true) in setref(c, false) end") "num")
              (check-equal? (tipo-de "let a = newref(true) b = newref(false) in swap(a, b) end") "bool"))
   (test-case "begin tiene el tipo de su ultima expresion"
              (check-equal? (tipo-de "begin 1; true end") "bool")
              (check-equal? (tipo-de "let c = newref(1) in begin setref(c, 2); newref(true) end end") "refto(bool)"))
   (test-case "letrec con una y con dos funciones"
              (check-equal? (tipo-de "letrec num f (k : num) = if ==(k, 0) then 0 else apply f (sub1(k)) end
                                      in apply f (3) end") "num")
              (check-equal? (tipo-de "letrec bool par (k : num) = if ==(k, 0) then true else apply impar (sub1(k)) end
                                             bool impar (k : num) = if ==(k, 0) then false else apply par (sub1(k)) end
                                      in apply par (7) end") "bool")
              (check-equal? (tipo-de "letrec num f (k : num) = 1 in f end") "(num -> num)"))
   (test-case "applyref con dos y con tres celdas"
              (check-equal? (tipo-de "let a = newref(1) b = newref(2)
                                      in let f = proc (u : refto(num), v : refto(num)) swap(u, v) end
                                         in applyref f (a, b) end end") "num")
              (check-equal? (tipo-de "let a = newref(1) b = newref(true)
                                      in let f = proc (u : refto(num), v : refto(bool)) deref(v) end
                                         in applyref f (a, b) end end") "bool"))
   (test-case "case"
              (check-equal? (tipo-de "case 3 of 1 => true of 3 => false otherwise => true end") "bool"))
   (test-case "el programa del enunciado, con la correccion minima (apply en lugar de applyref)"
              (check-equal? (tipo-de "let saldo = newref(100)
in
  let retirar = proc (cuenta : refto(num), monto : num)
                  if >=(deref(cuenta), monto)
                  then begin
                         setref(cuenta, -(deref(cuenta), monto));
                         deref(cuenta)
                       end
                  else deref(cuenta)
                  end
                end
  in
    begin
      apply retirar (saldo, q);
      deref(saldo)
    end
  end
end") "num"))
   (test-case "chequear no evalua nada: el almacen sigue vacio"
              (begin (initialize-store!)
                     (tipo-de "let c = newref(0) in begin setref(c, 1); deref(c) end end")
                     (check-equal? (tamano-almacen) 0)))))

(define errores-del-chequeador
  (test-suite
   "Grupo - Parte 3: mas mensajes de error"
   (test-case "if con la condicion mal tipada, con refto y con procedimiento"
              (check-true (contiene? (mensaje-de "if newref(true) then 1 else 2 end")
                                     "La condicion del if debe ser bool, se encontro refto(bool)"))
              (check-true (contiene? (mensaje-de "if proc (x : num) x end then 1 else 2 end")
                                     "La condicion del if debe ser bool, se encontro (num -> num)")))
   (test-case "ramas del if con refto distintos"
              (check-true (contiene? (mensaje-de "if true then newref(1) else newref(true) end")
                                     "Las ramas del if deben coincidir: refto(num) vs refto(bool)")))
   (test-case "apply sobre un booleano y sobre una referencia"
              (check-true (contiene? (mensaje-de "apply true (1)")
                                     "Se intento aplicar un valor de tipo bool"))
              (check-true (contiene? (mensaje-de "apply newref(1) (1)")
                                     "Se intento aplicar un valor de tipo refto(num)")))
   (test-case "aridad: de mas y de menos"
              (check-true (contiene? (mensaje-de "let f = proc (a : num) a end in apply f (1, 2) end")
                                     "El procedimiento espera 1 argumentos y recibio 2"))
              (check-true (contiene? (mensaje-de "let f = proc () 1 end in apply f (1) end")
                                     "El procedimiento espera 0 argumentos y recibio 1")))
   (test-case "argumento equivocado en la segunda posicion"
              (check-true (contiene? (mensaje-de "let f = proc (a : num, b : bool) a end in apply f (1, 2) end")
                                     "El argumento 2 debe ser bool, se encontro num")))
   (test-case "apply pasa una celda a un parametro num"
              (check-true (contiene? (mensaje-de "let f = proc (a : num) a end in apply f (newref(1)) end")
                                     "El argumento 1 debe ser num, se encontro refto(num)")))
   (test-case "deref sobre bool y sobre un procedimiento"
              (check-true (contiene? (mensaje-de "deref(true)") "deref espera refto(t), se encontro bool"))
              (check-true (contiene? (mensaje-de "deref(proc (x : num) x end)")
                                     "deref espera refto(t), se encontro (num -> num)")))
   (test-case "setref con celda de bool y con celda de celdas"
              (check-true (contiene? (mensaje-de "let c = newref(true) in setref(c, 1) end")
                                     "La celda guarda bool y se intento escribir num"))
              (check-true (contiene? (mensaje-de "let c = newref(newref(1)) in setref(c, 1) end")
                                     "La celda guarda refto(num) y se intento escribir num")))
   (test-case "setref y swap sobre algo que no es una referencia"
              (check-true (contiene? (mensaje-de "setref(5, 1)") "setref espera refto(t), se encontro num"))
              (check-true (contiene? (mensaje-de "swap(1, newref(2))") "swap espera refto(t), se encontro num"))
              (check-true (contiene? (mensaje-de "swap(newref(2), true)") "swap espera refto(t), se encontro bool")))
   (test-case "swap entre celdas de tipos distintos"
              (check-true (contiene? (mensaje-de "let a = newref(1) b = newref(true) in swap(a, b) end")
                                     "swap exige celdas del mismo tipo: refto(num) vs refto(bool)")))
   (test-case "letrec: el cuerpo no da el tipo declarado"
              (check-true (contiene? (mensaje-de "letrec bool f (k : num) = k in apply f (1) end")
                                     "El procedimiento f declara bool y su cuerpo da num"))
              (check-true (contiene? (mensaje-de "letrec num f (k : num) = 1 num g (k : num) = true in 0 end")
                                     "El procedimiento g declara num y su cuerpo da bool")))
   (test-case "identificador no declarado, incluso dentro de un cuerpo"
              (check-true (contiene? (mensaje-de "let a = 1 in +(a, b) end") "Variable no declarada: b"))
              (check-true (contiene? (mensaje-de "proc (x : num) y end") "Variable no declarada: y"))
              (check-true (contiene? (mensaje-de "let f = proc (x : num) x end in x end")
                                     "Variable no declarada: x")))
   (test-case "una variable de un let no es visible en las otras partes derechas"
              (check-true (contiene? (mensaje-de "let a = 1 b = a in b end")
                                     "Variable no declarada: a")))
   (test-case "el programa del enunciado, tal como esta escrito, no pasa el chequeador"
              (check-true (contiene? (mensaje-de "let saldo = newref(100)
in
  let retirar = proc (cuenta : refto(num), monto : num)
                  if >=(deref(cuenta), monto)
                  then begin
                         setref(cuenta, -(deref(cuenta), monto));
                         deref(cuenta)
                       end
                  else deref(cuenta)
                  end
                end
  in
    begin
      applyref retirar (saldo, q);
      deref(saldo)
    end
  end
end")
                                     "applyref espera refto(t) en el argumento 2, se encontro num")))
   (test-case "applyref: argumento refto de otro tipo y aridad"
              (check-true (contiene? (mensaje-de "let b = newref(true)
                                                  in let f = proc (u : refto(num)) 0 end
                                                     in applyref f (b) end end")
                                     "El argumento 1 debe ser refto(num), se encontro refto(bool)"))
              (check-true (contiene? (mensaje-de "let a = newref(1)
                                                  in let f = proc (u : refto(num), v : refto(num)) 0 end
                                                     in applyref f (a) end end")
                                     "El procedimiento espera 2 argumentos y recibio 1")))
   (test-case "case: control no numerico y ramas de tipos distintos"
              (check-true (contiene? (mensaje-de "case true of 1 => 1 otherwise => 0 end")
                                     "El valor de control del case debe ser num, se encontro bool"))
              (check-true (contiene? (mensaje-de "case n of 1 => true otherwise => 0 end")
                                     "Las ramas del case deben coincidir: num vs bool")))
   (test-case "primitivas con argumentos de tipo equivocado"
              (check-true (contiene? (mensaje-de "+(1, true)")
                                     "El argumento 2 de la primitiva + debe ser num, se encontro bool"))
              (check-true (contiene? (mensaje-de "not(1)")
                                     "El argumento 1 de la primitiva not debe ser bool, se encontro num"))
              (check-true (contiene? (mensaje-de "+(newref(1), 1)")
                                     "El argumento 1 de la primitiva + debe ser num, se encontro refto(num)")))))

;; --- Los dos lados se corresponden ----------------------------------

(define chequeo-y-ejecucion
  (test-suite
   "Grupo - solidez en ejemplos: el valor tiene el tipo que se anuncio"
   (test-case "un programa bien tipado produce un valor del tipo anunciado"
              (begin
                (check-equal? (tipo-de "+(n, m)") "num")
                (check-true (number? (corre "+(n, m)")))
                (check-equal? (tipo-de ">(n, m)") "bool")
                (check-true (boolean? (corre ">(n, m)")))
                (check-equal? (tipo-de "newref(1)") "refto(num)")
                (check-true (referencia? (corre "newref(1)")))
                (check-equal? (tipo-de "proc (x : num) x end") "(num -> num)")
                (check-true (procval? (corre "proc (x : num) x end")))))
   (test-case "los programas que el chequeador rechaza fallan al ejecutarse"
              (begin
                (check-true (fallo-con? (lambda () (corre "deref(5)")) "deref"))
                (check-true (contiene? (mensaje-de "deref(5)") "deref espera refto(t)"))
                (check-true (fallo-con? (lambda () (corre "if 1 then 2 else 3 end")) "booleana"))
                (check-true (contiene? (mensaje-de "if 1 then 2 else 3 end") "La condicion del if debe ser bool"))
                (check-true (fallo-con? (lambda () (corre "apply 5 (1)")) "no es un procedimiento"))
                (check-true (contiene? (mensaje-de "apply 5 (1)") "Se intento aplicar un valor de tipo num"))))
   (test-case "el chequeador es mas estricto que el evaluador: rechaza un if con ramas distintas que sí corre"
              (begin
                (check-equal? (corre "if >(n, m) then 1 else false end") 1)
                (check-true (contiene? (mensaje-de "if >(n, m) then 1 else false end")
                                       "Las ramas del if deben coincidir: num vs bool"))))))

(define fallidas-del-grupo
  (+ (run-tests almacen-tad)
     (run-tests expresiones-nuevas)
     (run-tests errores-de-referencias)
     (run-tests valor-y-referencia)
     (run-tests errores-de-applyref)
     (run-tests sin-regresion)
     (run-tests tipos-y-ambiente)
     (run-tests aciertos-del-chequeador)
     (run-tests errores-del-chequeador)
     (run-tests chequeo-y-ejecucion)))

(eopl:printf "~%==========================================~%")
(eopl:printf "  pruebas fallidas del grupo: ~s~%" fallidas-del-grupo)
(eopl:printf "  pruebas fallidas (fork + grupo): ~s~%" (+ fallidas fallidas-del-grupo))
(eopl:printf "==========================================~%")