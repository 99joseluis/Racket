#lang plai
;; Lenguaje FWAE
;; <expr> ::= <id>
;;          |<num>
;;          |{+ <expr> <expr>}
;;          |{- <expr> <expr>}
;;          |{with {<id> <expr>} <expr>}
;;          |{fun {<id>} <expr>}
;;          |{<expr> <expr>}

;; Sintaxis abstracta
(define-type FWAE
  [id (i symbol?)]
  [num (n number?)]
  [add (izq FWAE?) (der FWAE?)]
  [sub (izq FWAE?) (der FWAE?)]
  [with (id symbol?) (value FWAE?) (body FWAE?)]
  [fun (param symbol?) (body FWAE?)]
  [app (fun-expr FWAE?) (arg FWAE?)])

;; Substitucion
(define (subst expr sub-id val)
  (type-case FWAE expr
    [id (i) (if (symbol=? i sub-id) val expr)]
    [num (n) expr]
    [add (i d) (add (subst i sub-id val) (subst d sub-id val))]
    [sub (i d) (sub (subst i sub-id val) (subst d sub-id val))]
    [fun (p b)
         (if (symbol=? sub-id p)
             (fun p b)
             (fun p (subst sub-id val)))]
    [app (f a)
         (app (subst f sub-id val)
             


;; Interprete
;; interp: FWAE -> Fwae
(define (interp expr)
  (type-case FWAE expr
    [id (i) (error 'interp "Variable libre")]
    [num (n) expr]
    [add (i d) (num (+ (num-n (interp i)) (num-n (interp d))))]
    [sub (i d) (num (- (num-n (interp i)) (num-n (interp d))))]
    [with (i v b)
          (interp (subst b i v))]
    [fun (p b) expr]
    [app (f a)
         (interp (subst (fun-body f) (fun-param f) (interp a)))]))


(define expr
  (with 'foo (fun 'x (add (id x) (num 2)))
        (app (id 'foo) (add (num 3) (num 4)))))