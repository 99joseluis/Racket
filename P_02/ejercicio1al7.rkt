#lang plai

;; Ejercicio 1
;; Función que recibe n, r y devuelve el conjunto con los primeros r múltiplos de n.
;; multiplos: number number -> (listof number)
(define (multiplos n r)
  (map (lambda (x) (* x n)) (range 1 (+ r 1))))

;; Ejercicio 2
;; Predicado que nos dice si un número m es divisor de otro número n.
;; Si el parámetro recibido es cero, se devuelve un error.
;; divisor?: number number -> number
(define (divisor? m n)
  (if (equal? m 0) (error "El divisor no puede ser cero")
      (integer? (/ n m))))
      

;; Ejercicio 3
;; Función que nos da el una lista de divisores de un número pasado como parámetro
;; divisores: number -> (listof number)
(define (divisores n)
  (filter (lambda (x) (integer? (/ n x))) (range 1 (+ n 1))))

;; Ejercicio 4
;; Función que recibe un elemento a, una lista l y decide si a pertenece a l.
;; pertenece: a (listof a) -> boolean
(define (pertenece? a l)
  (findf (lambda (bol) (equal? bol #t)) (map (lambda (i) (equal? i a)) l)))

;; Ejercicio 5
;; Función que recibe una lista l con elementos. Devuelve una lista sin repeticiones con los elementos de l.
;; eliminaRep: (listof a) -> (listof a)
(define (eliminaRepetidos lista)
  (foldr (lambda (elemento lista)
           (if (not (pertenece? elemento lista))
               (cons elemento lista) lista)) '() lista))


;; Estructura que nos permite modelar puntos en el plano.
;; Sirve para modelar figuras geométricas.
(struct Punto (x y) #:inspector #f)

;; Ejercicio 6
;; Funcion que nos permite calcular el punto equidistante entre dos puntos.
;; Si alguno de los dos parámetros recibidos no es un punto, lanza un error
;; punto-medio: Punto Punto -> Punto
(define (punto-medio p q)
  (if (not(Punto? p))(error "P no es un punto" )
   (if (not(Punto? q))(error  "Q no es un punto" ) 
  (Punto(quotient(+ (Punto-x p) (Punto-x q)) 2) (quotient(+ (Punto-y p)(Punto-y q)) 2)))))

;; Ejercicio 7
;; Funcion que nos permite calcular la distancia entre dos puntos.
;; Si alguno de los dos parámetros recibidos no es un punto, lanza un error
;; distancia: Punto Punto -> number
(define (distancia p q)
  (if (not(Punto? p))(error "P no es un punto" )
   (if (not(Punto? q))(error  "Q no es un punto" ) 
  (sqrt(+ (expt (- (Punto-x p) (Punto-x q)) 2) (expt (- (Punto-y p) (Punto-y q)) 2) )))))

;; Ejercicio 8
;; Definición del tipo abstracto de datos Figura
(define-type Figura
  [Circulo ... ]
  [Triangulo ... ]
  [Rectangulo ... ]
  [Cuadrado ...])

;; Ejercicio 9
;; Función que recibe una figura y calcula su perímetro.
;; perimetro: Figura -> number
(define (perimetro fig)...)

;; Ejercicio 10
;; Función que recibe una figura y calcula su área.
;; area: Figura -> number
(define (area fig) ...)

;; Punto extra
;; Función que nos da el elemento más repetido en una lista. 
;; Si hay dos o más elementos repetidos el mismo número de veces, devuelve el primero en aparecer de izquierda a derecha.
;; masRepetido (listof a) -> number
;(define (masRepetido lista)


(define a (Punto 2 2))
(define b (Punto 2 8))
(define c (Circulo (Punto 0 0) 1))


(test (perimetro c) 6.283185307179586)
(test (distancia a b) 6)
(test (punto-medio a b) (Punto 2 5))




