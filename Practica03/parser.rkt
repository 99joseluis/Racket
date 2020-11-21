#lang plai
(require (file "./grammars.rkt"))

;; Toma una lista de números, symbolos o listas
;; y la traduce a un árbol de sintaxis abstracta WAE
;; A::=<number>
;;    | <symbol>
;;    | listof(A)
;; parse: A -> WAE
;; parse: s-expression -> WAE
(define (parse sexp)
  (cond
    [(empty? sexp) (error "Exp vacía")]
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(list? sexp)
     (case (car sexp)
       [(+) (if (< (length sexp) 3) (error "Argumento invalido") (op + (parse (cdr sexp))))]
       [(-) (if (< (length sexp) 3) (error "Argumento invalido") (op - (parse (cdr sexp))))]
       [(*) (if (< (length sexp) 3) (error "Argumento invalido") (op * (parse (cdr sexp))))]
       [(/) (if (< (length sexp) 3) (error "Argumento invalido") (op / (parse (cdr sexp))))]
       [(modulo) (if (< (length sexp) 3) (error "Argumento invalido") (op modulo (parse (cdr sexp))))]
       [(expt) (if (< (length sexp) 3) (error "Argumento invalido") (op expt (parse (cdr sexp))))]
       [(add1) (if (< (length sexp) 2) (error "Argumento invalido") (op add1 (parse (cdr sexp))))]
       [(sub1) (if (< (length sexp) 2) (error "Argumento invalido") (op sub1 (parse (cdr sexp))))]
       [(with) (with (for/list ([i (car (cdr sexp))]) (binding (car i) (parse (cadr i))))
                (parse (cadr (cdr sexp))))]
       [(with*) (with* (for/list ([i (car (cdr sexp))]) (binding (car i) (parse (cadr i))))
                 (parse (cadr (cdr sexp))))]
       [else (for/list ([i sexp]) (parse i))])]))