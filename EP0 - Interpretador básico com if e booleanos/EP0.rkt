#lang plai-typed

#|
Renan Ryu Kajihara
NUSP: 14605762
EP0 - MAC0316
|#

(define-type ArithC
  [numC (n : number)]
  [boolC (b : boolean)]
  [andC (l : ArithC) (r : ArithC)]
  [plusC (l : ArithC) (r : ArithC)]
  [multC (l : ArithC) (r : ArithC)]

  ;operações incluídas neste EP:
  
  [divC (l : ArithC) (r : ArithC)]
  [orC (l : ArithC) (r : ArithC)]
  [eqC (l : ArithC) (r : ArithC)]
  [lessC (l : ArithC) (r : ArithC)]
  [notC (l : ArithC)]
  [ifC  (c : ArithC)  (t : ArithC) (e : ArithC)]
  )



(define-type ArithS
  [numS    (n : number)]
  [boolS   (b : boolean)]
  [andS    (l : ArithS) (r : ArithS)] 
  [plusS   (l : ArithS) (r : ArithS)]
  [multS   (l : ArithS) (r : ArithS)]
  [bminusS (l : ArithS) (r : ArithS)]

  ;operações incluídas neste EP:
  
  [uminusS    (l : ArithS)] ;açúcar sintático
  [divS   (l : ArithS) (r : ArithS)]
  [orS    (l : ArithS) (r : ArithS)]
  [eqS   (l : ArithS) (r : ArithS)]
  [lessS   (l : ArithS) (r : ArithS)]
  [notS    (l : ArithS)]
  [greS   (l : ArithS) (r : ArithS)] ;açúcar sintático
  [ifS  (c : ArithS) (t : ArithS) (e : ArithS)])

  
(define (desugar [as : ArithS]) : ArithC  
  (type-case ArithS as
    [numS    (n)   (numC n)]
    [boolS   (b)   (boolC b)]
    [notS    (l) (notC (desugar l))]
    [andS    (l r) (andC (desugar l) (desugar r))]
    [eqS    (l r) (eqC (desugar l) (desugar r))]
    [plusS   (l r) (plusC (desugar l) (desugar r))] 
    [multS   (l r) (multC (desugar l) (desugar r))]
    [lessS  (l r)  (lessC (desugar l) (desugar r))]
    [orS  (l r)  (orC (desugar l) (desugar r))]
    [divS   (l r) (divC (desugar l) (desugar r))]
    [ifS (c t e) (ifC (desugar c) (desugar t) (desugar e))]
    
    ;Abaixo todos os operadores que são açúcar sintático
     
    [bminusS (l r) (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (l) (multC (desugar l) (numC -1))]
    [greS (l r) (andC(notC (lessC (desugar l)(desugar r))) (notC(eqC (desugar l) (desugar r))))]
    ))


(define (interp [a : ArithC]) : number
  (type-case ArithC a
    [numC   (n) n]
    [boolC  (b) (if b 1 0)]
    [andC  (l r)(if (and (not (=  (interp l) 0)) (not (= (interp r) 0))) 1 0)]
    [plusC  (l r) (+ (interp l) (interp r))]
    [multC  (l r) (* (interp l) (interp r))]

    ;operações incluídas neste EP:
    [divC (l r) (/ (interp l) (interp r))]
    [orC  (l r)(if (and (=  (interp l) 0) (= (interp r) 0)) 0 1)]
    [eqC  (l r)(if (=  (interp l) (interp r)) 1 0)]
    [lessC  (l r) (if (< (interp l) (interp r)) 1 0)]
    [notC  (l)(if (=  (interp l) 0) 1 0)]
    [ifC    (c t e) (if (not (= (interp c) 0))
                        (interp t)
                        (interp e))]))


(define (parse [s : s-expression]) : ArithS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-boolean? s) (boolS (s-exp->boolean s))]
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(and) (andS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]

         ;operações incluídas neste EP
         [(=) (eqS (parse (second sl)) (parse (third sl)))]
         [(not) (notS (parse (second sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(or) (orS (parse (second sl)) (parse (third sl)))]
         [(<) (lessS  (parse (second sl)) (parse (third sl)))]
         [(>) (greS  (parse (second sl)) (parse (third sl)))]
         [(/) (divS (parse (second sl)) (parse (third sl)))]
         [(if)(ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))

(define (interpS [ s : ArithS] ) : number
  (interp (desugar s)))

#|
Testes realizados:

> (interpS(parse '(/ 4 4)))
- number
1
> (interpS(parse '(/ 6 4)))
- number
1 1/2
> (interpS(parse '(or 1 0)))
- number
1
> (interpS(parse '(or 1 1)))
- number
1
> (interpS(parse '(or 0 0)))
- number
0
> (interpS (parse '(>(+ 5 (~ 3))10)))
- number
0
> (interpS (parse '(> 1 1)))
- number
0
> (interpS (parse '(> 20 1)))
- number
1
> (interpS (parse '(not(> 1 1))))
- number
1
> (interpS (parse '(+ 5 (~ 3))))
- number
2
>  (interpS (parse '(if (= (+ (~ 5) 7) (+ 1 1)) (+ 8 2) (/ 4 2))))
- number
10
>  (interpS (parse '#t))
- number
1
>  (interpS (parse '#f))
- number
0
> (interpS (parse '(= 5 6)))
- number
0
> (interpS (parse '(and (< 5 6) (> 10 9))))
- number
1
> (interpS (parse '(not (= 5 6))))
- number
1
> (parse '(not (= 5 6)))
- ArithS
(notS (eqS (numS 5) (numS 6)))
>  (parse '(- 5 6))
- ArithS
(bminusS (numS 5) (numS 6))
>  (desugar (parse '(- 5 6)))
- ArithC
(plusC (numC 5) (multC (numC -1) (numC 6)))
> (parse '(~ 4))
- ArithS
(uminusS (numS 4))


|#
