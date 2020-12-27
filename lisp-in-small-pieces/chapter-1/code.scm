;; The code here represents a Lisp1, aka Scheme, interpreter
(define wrong error) ;; alias 'wrong' to 'error'

(define (atom? exp) (not (pair? exp)))
(define (evaluate e env)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong "Cannot evaluate" e)))
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (evaluate (cadr e) env)
                  (evaluate (caddr e) env)
                  (evaluate (cadddr e) env)))
        ((begin) (eprogn (cdr e) env))
        ((set!) (update! (cadr e) env (evaluate (caddr e) env)))
        ((lambda) (make-function (cadr e) (cddr e) env))
        (else (invoke (evaluate (car e) env)
                      (evlis (cdr e) env))))))


(define (eprogn exps env)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (evaluate (car exps) env)
                 (eprogn (cdr exps) env))
          (evaluate (car exps) env))
      empty-begin))
(define empty-begin 813)

;; 'evlis' takes a list of expressions and returns a list of corresponding values of those expressions
(define (evlis exps env)
  (if (pair? exps)
      (cons (evaluate (car exps) env)
            (evlis (cdr exps) env))
      '()))

;; extracting and modifying subparts of the environment are done with 'lookup' and 'update!'
(define (lookup id env)
  (if (pair? env)
      (if (eq? (caar env) id)
          (cdar env)
          (lookup id (cdr env)))
      (wrong "No such binding" id)))

(define (update! id env value)
  (if (pair? env)
      (if (eq? (caar env) id)
          (begin (set-cdr! (car env) value)
                 value)
          (update! id (cdr env) value))
      (wrong "No such binding" id)))

;; cosntruction and enrichment of the environment
(define env.init '())

(define (extend env variables values)
  (cond ((pair? variables)
         (if (pair? values)
             (cons (cons (car variables) (car values))
                   (extend env (cdr variables) (cdr values)))
             (wrong "Too less values")))
        ((null? variables)
         (if (null? values)
             env
             (wrong "Too much values")))
        ((symbol? variables) (cons (cons variables values) env))))

;; representing functions
(define (invoke fn args)
  (if (procedure? fn)
      (fn args)
      (wrong "Not a function" fn)))

(define (make-function variables body env)
  (lambda (values)
    (eprogn body (extend env variables values))))

;; shallow binding simulation
(define (s.make-function variables body env)
  (lambda (values current.env)
    (let ((old-bindings (map (lambda (var val)
                               (let ((old-value (getprop var 'apval)))
                                 (putprop var 'apval val)
                                 (cons var old-value)))
                             variables
                             values)))
      (let ((result (eprogn body current.env)))
        (for-each (lambda (b) (putprop (car b) 'apval (cdr b))) ;; this is setting the bindings to their values before the evaluation of the function in the current.env
                  old-bindings)
        result))))

(define (s.lookup id env)
  (getprop id 'apval))
(define (s.update! id env value)
  (putprop id 'apval value))


;; the global environment
(define env.global env.init)
(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (begin (set! env.global (cons (cons 'name 'void) env.global))
            'name))
    ((definitial name value)
     (begin (set! env.global (cons (cons 'name value) env.global))
            'name))))

(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
       (lambda (values)
         (if (= arity (length values))
             (apply value values) ;; the real 'apply' of Scheme (the language used to implement our language)
             (wrong "Incorrect arity"
                    (list 'name values))))))))

(definitial t #t)

(define the-false-value #f) ;; define a value for the false value
(definitial f the-false-value)
(definitial nil '())

;; define some working variables
(definitial foo)
(definitial bar)
(definitial fib)
(definitial fact)
(definitial list (lambda (values) values))

;; define some functions
(defprimitive cons cons 2)
(defprimitive car car 1)
(defprimitive cdr cdr 1)
(defprimitive set-cdr! set-cdr! 2)
(defprimitive + + 2)
(defprimitive - - 2)
(defprimitive * * 2)
(defprimitive eq? eq? 2)
(defprimitive < < 2)

;; starting the interpreter
(define (chapter1-scheme)
  (define (toplevel)
    (newline)
    (display "Eval: ")
    (display (evaluate (read) env.global))
    (toplevel))
  (toplevel))

(chapter1-scheme)
