#lang plai
;; Lenguaje WAE
;;<expr> ::= <id>
;;        | <num>
;;        | {+ <expr> <expr>}
;;        | {- <expr> <expr>}
;;        | {with {<id> <expr>} <expr>}


(define-type WAE
  [id (i symbol?)]
  [num (n number?)]
  [add (izq WAE?) (der WAE?)]
  [sub (izq WAE?) (der WAE?)]
  [with (id symbol?) (value WAE?) (body WAE?)])

(define (contiene? e l)
  (match l
    ['() #f]
    [(cons x xs) (or (equal? e x) (contiene? e xs))]))


(define (union-listas l1 l2)
  (match l1
    ['() l2]
    [(cons x xs)
     (if (contiene? x l2)
         (union-listas xs l2)
         (cons x (union-listas xs l2)))]))


(define (de-ligado expr)
  (type-case WAE expr
    [id (i) empty]
    [num (n) empty]
    [add (izq der) (union-listas (de-ligado izq) (de-ligado der))]
    [sub (izq der) (union-listas (de-ligado izq) (de-ligado der))]
    [with (id value body)
          (union-listas (list id)
                        (union-listas (de-ligado value) (de-ligado body)))]))

(define (esta-en e expr)
  (type-case WAE expr
    [id (i)
        (if (equal? e i) (list i)
            empty)]
    [num (n) empty]
    [add (izq der) (union-listas (esta-en e izq) (esta-en e der))]
    [sub (izq der) (union-listas (esta-en e izq) (esta-en e der))]
    [with (id value body)
          (union-listas (esta-en e body) (esta-en id body))]))


(define (ligada expr)
  (type-case WAE expr
    [id (i) empty]
    [num (n) empty]
    [add (izq der) (union-listas (ligada izq) (ligada der))]
    [sub (izq der) (union-listas (ligada izq) (ligada der))]
    [with (id value body)
          (union-listas (esta-en id value) (esta-en id body))]))

(define (pertenece e l)
  (match l
    ['() empty]
    [(cons x xs) (if (equal? e x)
                     (pertenece e xs)
                     (union-listas (list x) (pertenece e xs)))]))

(define (es-libre e expr)
  (type-case WAE expr
    [id (i)
        (if (equal? e i) empty
            (list i))]
    [num (n) empty]
    [add (izq der) (union-listas (es-libre e izq) (es-libre e der))]
    [sub (izq der) (union-listas (es-libre e izq) (es-libre e der))]
    [with (id value body)
          (pertenece e (union-listas (es-libre e body) (es-libre id body)))]))

(define (libre expr)
  (type-case WAE expr
    [id (i) empty]
    [num (n) empty]
    [add (izq der) (union-listas (libre izq) (libre der))]
    [sub (izq der) (union-listas (libre izq) (libre der))]
    [with (id value body)
          (union-listas (es-libre id value) (es-libre id body))]))
              