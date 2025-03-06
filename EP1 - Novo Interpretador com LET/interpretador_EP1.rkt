#lang plai-typed

; Nome: Renan Ryu Kajihara, NUSP: 14605762

#|
 | interpretador simples, sem variáveis ou funçõess
 |#

#| primeiro as expressões "primitivas", ou seja, diretamente interpretadas
 |#

(define-type ExprC
  [numC    (n : number)]
  [idC     (s : symbol)]
  [plusC   (l : ExprC) (r : ExprC)]
  [multC   (l : ExprC) (r : ExprC)]
  [lamC    (arg : symbol) (body : ExprC)]
  [appC    (fun : ExprC) (arg : ExprC)]
  [ifC     (cond : ExprC) (y : ExprC) (n : ExprC)]
  [consC   (car : ExprC) (cdr : ExprC)]; Creates cell with a pair
  [carC    (pair : ExprC)]; Gets 1st element of a pair
  [cdrC    (pair : ExprC)]; Gets 2nd element of a pair

; operações implementadas nesse EP
  [quoteC (s : symbol)]
  [letrecC (arg : ExprC) (exp : ExprC)])


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

  ;operações implementadas nesse EP
  [letS (arg : ExprS) (exp : ExprS)]
  [let*S (arg : ExprS) (exp : ExprS)]
  [letrecS (arg : ExprS) (exp : ExprS)]
  [quoteS (s : symbol)]
  )


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

    ;operações implementadas nesse EP
    [quoteS (s)         (quoteC s)]
    [letrecS (arg exp)  (letrecC (desugar arg) (desugar exp))]
    
    [letS (arg exp) 
      (type-case ExprS arg
        [consS (id val)
          (type-case ExprS id
            [idS (s) 
              (appC (lamC s (desugar exp)) (desugar val))]
            [else (error 'desugar "O primeiro argumento não é um nome de uma variável")])]
        [else (error 'desugar "O argumento de let não é um par")])]
    
     [let*S (arg exp)
           (type-case ExprS arg
             [consS (arg1 arg2)
                    (type-case ExprS arg1
                      [consS (id1 val1)
                             (type-case ExprS id1
                               [idS (s1)
                                    (type-case ExprS arg2
                                      [consS (id2 val2)
                                             (type-case ExprS id2
                                               [idS (s2)
                                                    (appC
                                            
                                                     (lamC s1
                                                           (appC
                                                            (lamC s2 (desugar exp))
                                                            (desugar val2))) 
                                                     (desugar val1))] 
                                               [else (error 'desugar "O primeiro argumento da segunda variável não é um nome de uma variável válida")])]
                                      [else (error 'desugar "A segunda variável possui argumentos errados")])]
                               [else (error 'desugar "O primeiro argumento da primeira variável não é um nome de uma variável válida")])]
                      [else (error 'desugar "A primeira variável possui argumentos errados")])]
              [else (error 'desugar "Os argumentos estão errados")])]
                
    ))



; We need a new value for the box
(define-type Value
  [numV  (n : number)]
  [closV (arg : symbol) (body : ExprC) (env : Env)]
  [consV (car : Value) (cdr : Value)]
  [symV  (s : symbol)] ; novo tipo, útil para o comando quote
  )


; Bindings associate symbol with Boxes
; we need this to be able to change the value of a binding, which is important
; to implement letrec.

(define-type Binding
        [bind (name : symbol) (val : (boxof Value))])


; Env remains the same, we only change the Binding
(define-type-alias Env (listof Binding))
(define mt-env empty)
(define extend-env cons)


; Storage's operations are similar to Env's
;   bind <-> cell
;   mt-env <-> mt-store
;   extend-env <-> override-store


; lookup changes its return type
(define (lookup [varName : symbol] [env : Env]) : (boxof Value); lookup returns the box, we need this to change the value later
       (cond
            [(empty? env) (error 'lookup (string-append (symbol->string varName) " não foi encontrado"))] ; livre (não definida)
            [else (cond
                    [(symbol=? varName (bind-name (first env)))   ; achou!
                     (bind-val (first env))]
                    [else (lookup varName (rest env))])]))        ; vê no resto



; Primitive operators
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


; Return type for the interpreter, Value


(define (interp [a : ExprC] [env : Env] ) : Value
  (type-case ExprC a
    [numC (n) (numV n) ]
    [idC (n)  (unbox (lookup n env))]; we need to unbox the value in the environment before using it
    [lamC (a b) (closV a b env) ]

    ; operações implementadas nesse EP, (quote e letrec no interp)
    [quoteC (s) (symV s)]

    [letrecC (arg exp)
             (type-case ExprC arg
               [consC (id val)
                      (type-case ExprC id
                        [idC (s)
                             (let ([cell (box (numV 0))])  
                               (let ([new-env (extend-env (bind s cell) env)])
                                 (begin
                                   (set-box! cell (interp val new-env)) 
                                   (interp exp new-env))))] 
                        [else (error 'interp "O primeiro argumento do letrec não é uma variável")])]
               [else (error 'interp "O argumento de letrec não é um par")])]

    ; application of function
    [appC (f a)
          (let ((closure (interp f env))
                (argvalue (interp a env)))
            (type-case Value closure
              [closV (parameter body env)
                     (interp body (extend-env (bind parameter (box argvalue)) env))]
              [else (error 'interp "operation app aplied to non-closure")]
              ))]
   
    ;I left plusC without error-checking
    [plusC (l r)
             (let ((left (interp l env))
                   (right (interp r env)))
               (num+ left right))]
    ;multC
    [multC (l r)
           (let ( (left (interp l env))
                  (right (interp r env)))
             ;in this case type cheking is a little different
             (if (numV? left)
                 (if (numV? right)
                     (num* left right)
                     (error 'interp "second argument of multiplication not a number value"))
                 (error 'interp "first argument of multiplication not a number value"))
                 )]
    ; ifC serializes
    [ifC (c s n) (type-case Value (interp c env)
                   [numV (value)
                        (if (zero? value)
                            (interp n env )
                            (interp s env ))]
                   [else (error 'interp "condition not a number")]
                   )]

    ; Working with lists
    [consC (b1 b2) (let ( (car (interp b1 env))
                          (cdr (interp b2 env)))
                     (consV car cdr))]
    [carC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       car]
                [else (error 'interp "car applied to non-cell")]
                )]
    [cdrC (c) (type-case Value (interp c env)
                [consV (car cdr)
                       cdr]
                [else (error 'interp "cdr applied to non-cell")]
                )]
                 

    ))


; Parser with funny instructions for boxes
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

         ;operações incluídas nesse EP
         [(quote) (quoteS (s-exp->symbol (second sl)))]
         [(letrec) 
          (let ([binding (second sl)])
            (if (s-exp-list? binding)
              (let ([bindings (s-exp->list binding)])
                (let ([l1 (first bindings)])                      
                  (let ([arg1 (parse(first (s-exp->list l1)))]
                        [val1 (parse (second (s-exp->list l1)))])                                           
                    (letrecS (consS arg1 val1) (parse (third sl))))))
                (error 'parse "Argumentos errados para o letrec")))]      
         [(let) 
          (let ([binding (second sl)])
            (if (s-exp-list? binding)
              (let ([bindings (s-exp->list binding)])
                (let ([l1 (first bindings)])                      
                  (let ([arg1 (parse(first (s-exp->list l1)))]
                        [val1 (parse (second (s-exp->list l1)))])                                           
                    (letS (consS arg1 val1) (parse (third sl))))))
                (error 'parse "Argumentos errados para o let")))]
         
         [(let*) 
          (let ([binding (second sl)])
            (if (s-exp-list? binding)
              (let ([bindings (s-exp->list binding)])
                (let ([l1 (first bindings)]
                      [l2 (second bindings)])
                  (let ([arg1 (parse(first (s-exp->list l1)))]
                        [val1 (parse (second (s-exp->list l1)))]
                        [arg2 (parse(first (s-exp->list l2)))]
                        [val2 (parse (second (s-exp->list l2)))])
                        
                    (let*S (consS(consS arg1 val1)(consS arg2 val2)) (parse (third sl))))))
                (error 'parse "Argumentos errados para o let*")))]
           
           [else (error 'parse "invalid list input")]))]
    [else (error 'parse "invalid input")]))


; Facilitator
(define (interpS [s : s-expression]) (interp (desugar (parse s)) mt-env))


; Examples
(interpS '(+ 10 (call (lambda x (car x)) (cons 15 16))))

(interpS '(call (lambda x (+ x 5)) 8))


(interpS '(call (lambda f (call f (~ 32))) (lambda x (- 200 x))))


; Tests

(display "Testes: \n")

(test (interp (carC (consC (numC 10) (numC 20)))
              mt-env)
      (numV 10))

(test (interpS '(let ((x 100)) x)) (numV 100))

(test (interpS '(let ((x 100)) (+ 1 x))) (numV 101))

(test (interpS '(let ((x 100)) (let ((y (+ x 1))) (+ x y)))) (numV 201))

(test (interpS '(let ((f (lambda x x))) (call f 10))) (numV 10))

(test (interpS '(let ((plus (lambda x (lambda y (+ x y)))))
    (call (call plus 10) 20) )) (numV 30))

(test (interpS '(let* ((x 100) (y (+ x 1))) (+ x y))) (numV 201))

(test (interpS 
    '(let* ((inc (lambda n (+ n 1))) (inc2 (lambda n (call inc (call inc n))))) (call inc2 10)))
    (numV 12))


(test (interpS '(letrec ((f (lambda n (if n (* 2 (call f (- n 1))) 1)))) (call f 10))) (numV 1024))

(test (interpS '(letrec ((f (lambda n (if n (* 2 (call f (- n 1))) 1)))) 10)) (numV 10))

(test (interpS '(letrec ((f (lambda n n))) (call f 7))) (numV 7))

(test (interpS '(quote alan)) (symV 'alan))