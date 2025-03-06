#lang plai-typed

; Nome: Renan Ryu Kajihara
; NUSP: 14605762
(define-type ExprC
  [numC    (n : number)]
  [idC     (s : symbol)]
  [plusC   (l : ExprC) (r : ExprC)]
  [multC   (l : ExprC) (r : ExprC)]
  [lamC    (arg : symbol) (body : ExprC)]
  [appC    (fun : ExprC) (arg : ExprC)]
  [ifC     (cond : ExprC) (y : ExprC) (n : ExprC)]
  [consC   (car : ExprC) (cdr : ExprC)]
  [carC    (pair : ExprC)]
  [cdrC    (pair : ExprC)]
  [equalC (l : ExprC) (r : ExprC)]
  [letrecC (s : symbol) (v : ExprC) (r : ExprC)]
  [quoteC  (s : symbol)]
  [read-loopC])

(define-type ExprS
  [numS    (n : number)]
  [idS     (s : symbol)]
  [lamS    (arg : symbol) (body : ExprS)]
  [appS    (fun : ExprS) (arg : ExprS)]
  [plusS   (l : ExprS) (r : ExprS)]
  [bminusS (l : ExprS) (r : ExprS)]
  [uminusS (e : ExprS)]
  [multS   (l : ExprS) (r : ExprS)]
  [ifS     (c : ExprS) (y : ExprS) (n : ExprS)]
  [consS   (car : ExprS) (cdr : ExprS)]
  [carS    (pair : ExprS)]
  [cdrS    (pair : ExprS)]
  [equalS   (l : ExprS) (r : ExprS)]
  [letS    (name : symbol)  (value : ExprS) (body : ExprS)]
  [let*S   (name1 : symbol) (value1 : ExprS) (name2 : symbol) (value2 : ExprS) (body : ExprS)]
  [letrecS (name : symbol)  (vlue : ExprS) (body : ExprS)]
  [quoteS  (name : symbol)]
  [read-loopS])


(define (desugar [as : ExprS]) : ExprC
  (type-case ExprS as
    [numS    (n)        (numC n)]
    [idS     (s)        (idC s)]
    [lamS    (a b)      (lamC a (desugar b))]
    [appS    (fun arg)  (appC (desugar fun) (desugar arg))]
    [plusS   (l r)      (plusC (desugar l) (desugar r))]
    [multS   (l r)      (multC (desugar l) (desugar r))]
    [bminusS (l r)      (plusC (desugar l) (multC (numC -1) (desugar r)))]
    [uminusS (e)        (multC (numC -1) (desugar e))]
    [ifS     (c y n)    (ifC (desugar c) (desugar y) (desugar n))]
    [consS   (b1 b2)    (consC (desugar b1) (desugar b2))]
    [carS    (c)        (carC (desugar c))]
    [cdrS    (c)        (cdrC (desugar c))]

    [letS    (name value body)    (appC (lamC name (desugar body)) (desugar value))]

    [let*S   (name1 value1 name2 value2 body)
             (appC (lamC name1 (appC (lamC name2 (desugar body)) (desugar value2))) (desugar value1))]

    [letrecS (name value body)    (letrecC name (desugar value) (desugar body))]

    [quoteS  (name)        (quoteC name)]
    [equalS   (l r)      (equalC (desugar l) (desugar r))]
    [read-loopS () (read-loopC)]))



(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [consV (car : (boxof Value)) (cdr : (boxof Value))]

  [symV  (s : symbol)]
  [suspV (exp : ExprC) (env : Env)]
  [boolV (b : boolean)])

(define (query-promise [promise : (boxof Value)]) : Value
  (type-case Value (unbox promise)
             [suspV (body susp-env)
                       (let* ((finalValue (interp body susp-env)))
                         (begin (set-box! promise finalValue)
                           finalValue))]
             [else (unbox promise)]))

(define-type Binding
        [bind (name : symbol) (val : (boxof Value))])


(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


(define (lookup [varName : symbol] [env : Env]) : (boxof Value)
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string varName) " não foi encontrado"))]
            [else (cond
                    [(symbol=? varName (bind-name (first env)))
                     (bind-val (first env))]
                    [else (lookup varName (rest env))])]))


(define (num+ [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (+ (numV-n l) (numV-n r)))]
        [else
             (error 'num+ "Um dos argumentos não é número")]))

(define (num* [l : Value] [r : Value]) : Value
    (cond
        [(and (numV? l) (numV? r))
             (numV (* (numV-n l) (numV-n r)))]
        [else
             (error 'num* "Um dos argumentos não é número")]))

(define (read-till-end)
  (let ((input (read)))
    (if (and (s-exp-symbol? input) (eq? (s-exp->symbol input) '@end))
      (begin (display "FINISHED-READLOOP\n") (numV 0))
      (begin (display "\ninterpret-command:")
        (display input)
        (display "\nresult:")
        (display (interpS input))
        (display "\n")
        (numV 0)
        (read-till-end)))))

(define (get-param closure)
  (type-case Value closure
    [closV (param body env) param] ; Retorna o parâmetro da closure
    [else (error 'get-param "expected a closure")]))
(define (interp [a : ExprC] [env : Env] ) : Value
  (type-case ExprC a
    [numC (n) (numV n) ]
    [idC (n)  (query-promise (lookup n env))]
    [lamC (a b) (closV a b env) ]


    [appC (f a)
          (let ((fun-val (interp f env))      
                (arg-val (box (suspV a env)))) 
            (type-case Value fun-val
              [closV (par exp env1)                     
                     (interp exp (extend-env (bind par arg-val) env1))]              
              [else (error 'interp "app aplicado a um valor que não é uma função")]) )]


    [plusC (l r)
             (let ((left (interp l env))
                   (right (interp r env)))
               (num+ left right))]
    [multC (l r)
           (let ( (left (interp l env))
                  (right (interp r env)))

             (if (numV? left)
                 (if (numV? right)
                     (num* left right)
                     (error 'interp "second argument of multiplication not a number value"))
                 (error 'interp "first argument of multiplication not a number value"))
                 )]

    [ifC (c s n) (type-case Value (interp c env)
                   [numV (value)
                        (if (zero? value)
                            (interp n env )
                            (interp s env ))]
                   [else (error 'interp "condition not a number")]
                   )]

    [consC (b1 b2) (let ((car (box (suspV b1 env)))
                          (cdr (box (suspV b2 env))))
                     (consV car cdr))]
    [carC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       (query-promise car)]
                [else (error 'interp "car applied to non-cell")]
                )]
    [cdrC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       (query-promise cdr)]
                [else (error 'interp "cdr applied to non-cell")]
                )]

    [letrecC (name val body)
             (let* ([the-box (box (numV 0))]
                    [new-env (extend-env (bind name the-box) env)]
                    [thingy (interp val new-env)])

                   (begin
                     (set-box! the-box thingy)
                     (interp body new-env)))]

    [quoteC (s) (symV s)]
    [equalC (l r)
            (let ([left (query-promise (box (interp l env)))]
             [right (query-promise(box (interp r env)))])
         (if (equal? left right)
             (boolV #t)
             (boolV #f)))]
    [read-loopC () (read-till-end)]))


(define (parse [s : s-expression]) : ExprS
  (cond
    [(s-exp-number? s) (numS (s-exp->number s))]
    [(s-exp-symbol? s) (idS (s-exp->symbol s))] ; pode ser um símbolo livre nas definições de função
    [(s-exp-list? s)
     (let ([sl (s-exp->list s)])
       (case (s-exp->symbol (first sl))
         [(+) (plusS (parse (second sl)) (parse (third sl)))]
         [(*) (multS (parse (second sl)) (parse (third sl)))]
         [(-) (bminusS (parse (second sl)) (parse (third sl)))]
         [(~) (uminusS (parse (second sl)))]
         [(lambda) (lamS (s-exp->symbol (second sl)) (parse (third sl)))] ; definição
         [(call) (appS (parse (second sl)) (parse (third sl)))]
         [(if) (ifS (parse (second sl)) (parse (third sl)) (parse (fourth sl)))]
         [(cons) (consS (parse (second sl)) (parse (third sl)))]
         [(car) (carS (parse (second sl)))]
         [(cdr) (cdrS (parse (second sl)))]
         [(equal?) (equalS (parse (second sl)) (parse (third sl)))]
         ;; new implementation
         [(let) (let ([terms (s-exp->list (first (s-exp->list (second sl))))])
                  (letS (s-exp->symbol (first terms))
                      (parse (second terms))
                      (parse (third sl))))]

         [(let*) (let ([first-term (s-exp->list (first (s-exp->list (second sl))))]
                       [second-term (s-exp->list (second (s-exp->list (second sl))))])
                   (let*S (s-exp->symbol (first first-term))
                        (parse (second first-term))
                        (s-exp->symbol (first second-term))
                        (parse (second second-term))
                        (parse (third sl))))]
         [(letrec) (let ([terms (s-exp->list (first (s-exp->list (second sl))))])
                  (letrecS (s-exp->symbol (first terms))
                      (parse (second terms))
                      (parse (third sl))))]

         [(quote) (quoteS (s-exp->symbol (second sl)))]

         [(read-loop) (read-loopS)]

         [else (error 'parse
                      (string-append
                        "invalid list input: unrecognized expression start: "
                        (symbol->string (s-exp->symbol (first sl)))))]))]

    [else (error 'parse "invalid input: unknown s-expression type")]))


(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))

;; Testes do EP2

(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (let ((death (call halt 0))) 1))) 
      (numV 1))

(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (car (cons 2 (call halt 0))))) 
      (numV 2))

(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (cdr (cons (call halt 0) 3)))) 
      (numV 3))

(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (let ((first (lambda x (lambda y x))))
                    (call (call first 4) (call halt 0))))) 
      (numV 4))

(test (interpS '(letrec ((halt (lambda x (call halt x))))
                  (let ((second (lambda x (lambda y y))))
                    (call (call second (call halt 0)) 5)))) 
      (numV 5))

(test (interpS '(letrec ((iterate (lambda f (lambda x (cons x (call (call iterate f) (call f x)))))))
                 (let ((things (call (call iterate (lambda x (* 10 x))) 1)))

                   (+ (+ (car things) (car (cdr things)))
                      (car (cdr (cdr things)))))

                 )) 
      (numV 111))

(test (interpS '(letrec ((drop (lambda n (lambda xs (if n (call (call drop (- n 1)) (cdr xs)) xs)))))
                  (letrec ((iterate (lambda f (lambda x (cons x (call (call iterate f) (call f x)))))))
                    (let ((things (call (call drop 3) (call (call iterate (lambda x (* 10 x))) 1))))

                      (+ (+ (car things) (car (cdr things)))
                         (car (cdr (cdr things)))))
                    
                    ))) 
      (numV 111000))

(test (interpS '(equal? 1 1))
      (boolV #t))

(test (interpS '(equal? 1 2))
      (boolV #f))

(test (interpS '(equal? (+ 2 2) 4))
      (boolV #t))


