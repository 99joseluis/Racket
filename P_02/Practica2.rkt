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
  (match lista
    ['() empty]
    [(cons x xs) (cons x (eliminaRepetidos (filter (lambda (y) (not (equal? x y))) xs)))]
    ))


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
  (Punto(/ (+ (Punto-x p) (Punto-x q)) 2) (/ (+ (Punto-y p)(Punto-y q)) 2)))))

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
  ;Crear constructores de cada tipo
  [Circulo (p Punto?) (radio positive?)]
  [Triangulo (p1 Punto?) (p2 Punto?) (p3 Punto?)]
  [Rectangulo (p Punto?) (base positive?) (alt positive?) ] 
  [Cuadrado (p Punto?) (lado positive?)])

;; Ejercicio 9
;; Función que recibe una figura y calcula su perímetro.
;; perimetro: Figura -> number
(define (perimetro fig)
 (type-case Figura fig
  [Circulo (p radio) (* (* 2 pi) radio)] 
  [Triangulo (p1 p2 p3) (+ (distancia p1 p2) (distancia p1 p3) (distancia p2 p3))]   
  [Rectangulo (p base alt) (+ (expt 2 base) (expt 2 alt)) ]
  [Cuadrado (p lado) (* 4 lado)] 
  ))

;; Ejercicio 10
;; Función que recibe una figura y calcula su área.
;; area: Figura -> number
(define (area fig)
  (type-case Figura fig
   [Circulo (p radio) (* pi (* radio radio) )]
   [Triangulo (p1 p2 p3)(let ([s (/ (+ (distancia p1 p2) (distancia p1 p3)(distancia p2 p3)) 2)] [t 4])
                                                       (sqrt (* s (- s (distancia p1 p2)) (- s (distancia p1 p3)) (- s (distancia p2 p3)))))]
   [Rectangulo (p base alt) (/ (* base alt) 2)]
   [Cuadrado (p lado) (* lado lado)]
    ))  

;; Punto extra
;; Función que nos da el elemento más repetido en una lista. 
;; Si hay dos o más elementos repetidos el mismo número de veces, devuelve el primero en aparecer de izquierda a derecha.
;; masRepetido (listof a) -> number
(define (masRepetido lista)
  (lambda (x) (

;;Funcion que te da la suma de las repeticiones de l en una lista
;; repetido symbol (listof ) -> int
(define (repetido l lista)
  (match lista
    ['() 0]
    [(cons x xs) (if (equal? x l)
                     (+ 1 (repetido l (cdr lista)))
                     (repetido l (cdr lista)))]))



(define a (Punto 2 2))
(define b (Punto 2 8))
(define c (Circulo (Punto 0 0) 1))
(define d (Circulo (Punto 0 0) 2))


;;(test (perimetro c) 6.283185307179586)
(test (distancia a b) 6)
(test (punto-medio a b) (Punto 2 5))




  
