#lang scheme
(require racket/stream)

; Nome: Renan Ryu Kajihara, NUSP:14605762

;1a)

(define itera (lambda (f x)
                     (stream-cons x (itera f (f x)))))
                     

                        
(define soma1 (lambda (x)
             (+ x 1)))

;teste
(define 1a (itera soma1 0))
(display "Stream que começa com 0 e itera a função soma1 (6 primeiros termos): \n")
(display (stream-ref 1a 0))
(display "-")
(display (stream-ref 1a 1))
(display "-")
(display (stream-ref 1a 2))
(display "-")
(display (stream-ref 1a 3))
(display "-")
(display (stream-ref 1a 4))
(display "-")
(display (stream-ref 1a 5))

;1b)

(define ciclo (lambda (l)
                (if (null? l)
                    '()             
                    (stream-lazy (stream-append l (ciclo l))))))

;teste
(define 1b (ciclo '(1 2)))
(display "\n")
(display "Stream que repete o ciclo (1 2) (6 primeiros termos): \n")
(display (stream-ref 1b 0))
(display "-")
(display (stream-ref 1b 1))
(display "-")
(display (stream-ref 1b 2))
(display "-")
(display (stream-ref 1b 3))
(display "-")
(display (stream-ref 1b 4))
(display "-")
(display (stream-ref 1b 5))
;1c)
(define map2 (lambda (f l1 l2)
               (stream-cons (f (stream-first l1) (stream-first l2))
                               (map2 f (stream-rest l1) (stream-rest l2)))))
(define fib 
  (stream-cons 1 (stream-cons 1 (map2 (lambda (x y) (+ x y))
                       fib 
                       (stream-rest fib)))))

;teste
(display "\n")
(display "Stream que representa a sequência de Fibonacci (6 primeiros termos): \n") 
(display (stream-ref fib 0))
(display "-")
(display (stream-ref fib 1))
(display "-")
(display (stream-ref fib 2))
(display "-")
(display (stream-ref fib 3))
(display "-")
(display (stream-ref fib 4))
(display "-")
(display (stream-ref fib 5))

;2)
(define merge (lambda (l1 l2)
                (stream-cons (stream-first l1) (merge l2 (stream-rest l1)))))

;teste
(display "\n")
(display "Stream que faz o merge das streams do ex 1a e 1c (6 primeiros termos) : \n")
(define ex2 (merge 1a fib))
(display (stream-ref ex2 0))
(display "-")
(display (stream-ref ex2 1))
(display "-")
(display (stream-ref ex2 2))
(display "-")
(display (stream-ref ex2 3))
(display "-")
(display (stream-ref ex2 4))
(display "-")
(display (stream-ref ex2 5))
;3)
(define (foreach-inf l f)
  (if (null? l)
      '()  
      (let ((l1 (stream-first l))
            (l2 (stream-first (stream-rest l))))  
            (stream-cons (f (stream-first l1)) 
                  (foreach-inf (stream-cons l2 (stream (stream-rest l1))) f)))))
                                         





;teste
(display "\n")

(define inteirosde2 (stream-cons 2 (stream-map soma1 inteirosde2)))
(define inteirosde10 (stream-cons 10 (stream-map soma1 inteirosde10)))

(define res (foreach-inf (stream inteirosde2 inteirosde10) soma1))

(display "Stream que aplica a função soma1 na stream que possue os inteiros a partir de 2 e na stream que possue os inteiros a partir de 10, fazendo o merge dos resultados (6 primeiros termos) :\n")
(display (stream-ref res 0))
(display "-")
(display (stream-ref res 1))
(display "-")
(display (stream-ref res 2))
(display "-")
(display (stream-ref res 3))
(display "-")
(display (stream-ref res 4))
(display "-")
(display (stream-ref res 5))
(display "\n")

#|
Para esquemas de recursão da forma:
x0 = <valor inicial0>	
x1 = <valor inicial1>
x2 = <valor inicial2>
xi+3 = f(xi+2 ,xi+1 ,xi , i+3)
A fórmula geral seria:

(define tudo (cons 0 (mapcar (lambda (x) (+ x 1)) tudo)))

(define lista
  (cons x0
        (cons x1
              (cons x2
                    (mapcar4 f (cdr (cdr lista)) (cdr lista) lista (cdr (cdr (cdr tudo))))))))
|#