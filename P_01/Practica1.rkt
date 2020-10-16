#lang plai
;; Práctica 1 "Introducción a Racket"
;; @author: José Luis García Santamaria
;; @cuenta: 31617464-6
;; @date: 30-septiembre-2020

;;FUNCIONES PRIMARIAS

;;--------------------------------1------------------------------
;; Función que te dice si x número es par
;; esPar?: number -> boolean
(define (esPar? x)
  (integer? (/ x 2)))

;;--------------------------------2------------------------------
;; Función para obtener una lista de numeros en orden (de menor a mayor)
;; hasta llegar al numero deseado partiendo desde cero
;; n -> numero deseado
;; menores: number -> (listof number)
(define (menores n)
  (numera? n 0 (list) 1 ))

;;--------------------------------3------------------------------
;; Funcion para obtener la lista ordenada (de menor a mayor) de numeros pares
;; hasta el numero ingresado, si este no es par la lista llegara hasta el ultimo
;; numero par mas chico que el numero ingresado
;;pares: number -> (listof number)
(define (pares n)
  (numera? n 0 (list) 2 ))

;;--------------------------------4-----------------------------
;; Funcion que emplea la formula para sumar los primeros n numeros al cuadrado
;; suma-cuadrados: number -> number
;; Formula: (n(n+1)(2n+1))/6
(define (suma-cuadrados n)
  (/ (* (* (+ n 1) n) (+ (* 2 n) 1)) 6))

;;--------------------------------5-----------------------------
;; Funcion que emplea la formula de manera recursiva para sumar los primeros n numeros al cuadrado
;; suma-cuadrados: number -> number
;; Formula: (n(n+1)(2n+1))/6
(define (suma-cuadradosR n)
  (suma n 0 1))

;;--------------------------------6-----------------------------
;; Funcion para saber si la formula general de ecuaciones cuadraticas tiene raices positivas
;;raicesReales?: number number number -> boolean
(define (raicesReales? a b c)
  (if (real? (raiz? a b c))
  (if (or (> (raiz? a b c) 0) (equal? (raiz? a b c) 0))
  #t
  #f)
  #f))

;;--------------------------------7-----------------------------
;; Funcion para obtener X_1 de la formula general de ecuaciones cuadraticas
;; la raiz es positiva
;; general1: number number number -> number
(define (general1 a b c)
  (if (raicesReales? a b c)
      (/(+(- b) (raiz? a b c)) (* 2 a))
      (error "El polinomio no tiene raices reales")))

;;--------------------------------8-----------------------------
;; Funcion para obtener X_2 de la formula general de ecuaciones cuadraticas
;; la raiz es negativa
;; general2: number number number -> number
(define (general2 a b c)
  (if (raicesReales? a b c)
      (/(-(- b) (raiz? a b c)) (* 2 a))
      (error "El polinomio no tiene raices reales")))

;;--------------------------------9-----------------------------
;;Una función que recibe una lista como parámetro y nos devuelve una lista con el orden de sus
;;elementos invertidos.
;; reversa-lista: (listof a) -> (listof a)
(define (reversa-lista lista)
  (cond
    [(= (length lista) 0) null]
    [(= (length lista) 1) (cons (car lista) null)]
    [else (append (reversa-lista (cdr lista)) (list(car lista)))]))

;;--------------------------------10-----------------------------
;; Un predicado que verifique si una lista (no necesariamente homogénea) es un palíndromo.
;; palindromo-lista?: (listof a) -> boolean
(define (palindromo? lista)
  (if (equal? lista (reversa-lista lista))
      #t
      #f))

;; FUNCIONES AUXILIARES

;; --------------------------------------------------------------
;; Funcion recursiva para crear una lista de numeros (menor a mayor) hasta un numero deseado
;; n -> numero deseado
;; m -> numero donde iniciara la lista
;; l -> lista de numeros ordenados
;; i -> es un contador
;; numera?: number - number - (listof number) -> (listof number)
(define (numera? n m l i)
  (cond
    [(equal? n m) (append l (list m))]
    [(< m n) (numera? n (+ m i) (append l (list m)) i)]
    [(> m n) l]
    [else (error "Ocurrio un error")]))

;; --------------------------------------------------------------
;;Funcion para la suma de los primeros n numeros al cuadrado
(define (suma n s i)
  (if (= i n)
      (+ s (sqr i))
      (suma n (+ s (sqr i)) (+ i 1))))
      
;; --------------------------------------------------------------
;; Funcion para obtener la raiz de la formula general de ecuaciones cuadraticas
;; raiz?: number number number -> number
(define (raiz? a b c)
  (sqrt(- (expt b 2)(* (* 4 a) c))))
  
