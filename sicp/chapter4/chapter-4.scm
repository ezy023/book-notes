;; BEGIN GIVEN EVALUATOR CODE
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (if (no-operands? exps)
      '()
      (cons (eval (first-operand exps) env)
            (list-of-values (rest-operands exps) env))))

(define (eval-if exp env)
  (if (true? (eval (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (eval (assignment-value exp) env)
                       env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval (definition-value exp) env)
    env)
  'ok)


(define (list-of-values-l-r exps env)
  (if (no-operands? exp)
      '()
      (let ((lval (eval (first-operand exps) env)))
        (cons lval (list-of-values (rest-operands exps) env)))))

(define (list-of-values-r-l exps env)
  (if (no-operands? exp)
      '()
      (let ((rval (list-of-values (rest-operands exps) env)))
        (cons (eval (first-operand exps) env) rval))))

(define (self-evaluating? exp)
  (cond ((number? exp) true)
        ((string? exp) true)
        (else false)))

(define (variable? exp) (symbol? exp))

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp) (cadr exp))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; assignment
(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr  exp))

;; definition
(define (definition? exp)
  (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp)   ; formal parameters
                   (cddr exp)))) ; body

;; lambda
(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-paramenters exp) (cadr exp))
(define (lambda-body exp) (cddr exp))

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))

;; if
(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
      (cadddr exp)
      'false))

(define (make-if predicate consequent alternative)
  (list 'if predicate consequent alternative))

;; begin
(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions exp) (cdr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))

(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin seq) (cons 'begin seq))

;; Any expression that is not one of the above types is a procedure application
(define (application?  exp) (pair? exp))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))


;; cond
(define (cond? exp) (tagged-list? exp 'cond))

(define (cond-clauses exp) (cdr exp))
(define (cond-else-clause? clause)
  (eq? (cond-predicate clause) 'else))
(define (cond-predicate clause) (car clause))
(define (cond-actions clause) (cdr clause))

(define (cond->if exp)
  (expand-clauses (cond-clauses exp)))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last --  COND->IF"
                       clauses))
            (make-if (cond-predicate first)
                     (sequence->exp (cond-actions first))
                     (expand-clauses rest))))))


;; END GIVEN EVALUATOR CODE


;; 4.3
(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) exp))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (eval exp env)
  (put 'quote 'text-of-quotation)
  (put 'set! 'eval-assignment)
  (put 'define 'eval-definition)
  (put 'if 'eval-if)
  (put 'lambda '(lambda (e)
                  (make-procedure (lambda-parameters e)
                                  (lambda-body e)
                                  env)))
  (put 'begin '(lambda (e) (eval-sequence (begin-actions exp) env)))
  (put 'cond '(lambda (e) (eval (cond->if e) env)))
  ;; assuming we stick with the 'call' usage from ex 4.2 as that makes this simpler
  (put 'call '(lambda (e) (apply (eval (operator exp) env)
                                 (list-of-values (operands exp) env))))
  (define (procedure-tag exp)
    (car exp))
  (define (dispatch m)
    (if (self-evaluating? exp)
        exp
        (let ((proc (get m)))
          (if (not (null? proc))
              (proc exp)
              (error "Unknow expression type -- EVAL" exp)))))
  (dispatch (procedure-tag m)))

(define (and? exp) (tagged-list? exp 'and))
(define (and-predicates exp) (cdr exp))
(define (eval-and seq env)
  (if (null? seq)
      #t
      (if (last-exp? seq)
          (if (true? (eval (first-exp seq) env))
              (eval (first-exp seq) env)
              #f)
          (if (true? (eval (first-exp seq) env))
              (eval-and (rest-exps seq) env)
              #f))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-predicates exp) (cdr exp))
(define (eval-or seq env)
  (if (null? seq)
      #f
      (if (last-exp? seq)
          (if (true? (eval (first-exp seq) env))
              #t
              #f)
          (if (true? (eval (first-exp seq) env))
              #t
              (eval-or (rest-exps seq) env)))))

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ;; ...
        ;; add 'and' and 'or'
        ((and? exp) (eval-and (and-predicates exp) env))
        ((or? exp) (eval-or (or-predicates) env))
        ;; ...
        (else
         (error "Unknown expression type -- EVAL" exp))))

(eval-and (and-predicates '(and (> 1 0) (= 2 2) (< 2 3))) (the-environment))
(eval-and (and-predicates '(and (> 1 0) (= 2 3) (< 2 3))) (the-environment))
(eval-or (or-predicates '(or (> 0 1) (= 2 2) (< 3 2))) (the-environment))
(eval-or (or-predicates '(or (> 0 1) (= 2 3) (< 3 2))) (the-environment))

;; 'and and 'or as derived expressions
(define (and->if exp)
  (expand-and-predicates (and-predicates exp)))
(define (expand-and-predicates predicates)
  (let ((first (first-exp predicates))
        (rest (rest-exps predicates)))
    (if (null? rest)
        (make-if first #t #f)
        (make-if first (expand-and-predicates (cdr predicates)) #f))))

(define (or->if exp)
  (expand-or-predicates (or-predicates exp)))
(define (expand-or-predicates predicates)
  (let ((first (first-exp predicates))
        (rest (rest-exps predicates)))
    (if (null? rest)
        (make-if first #t #f)
        (make-if first #t (expand-or-predicates (cdr predicates))))))

(define (derived-eval-and exp env)
  (eval (and->if exp) env))

(define (derived-eval-or exp env)
  (eval (or->if exp) env))

(derived-eval-and (and-predicates '(and (> 1 0) (= 2 2) (< 2 3))) (the-environment))
(derived-eval-and (and-predicates '(and (> 1 0) (= 2 3) (< 2 3))) (the-environment))
(derived-eval-or (or-predicates '(or (> 0 1) (= 2 2) (< 3 2))) (the-environment))
(derived-eval-or (or-predicates '(or (> 0 1) (= 2 3) (< 3 2))) (the-environment))


;; Ex 4.5
(define (arg-pass-syntax? clause)
  (eq? (cadr clause) '=>))
(define (arg-pass-action clause)
  (caddr clause))

(define (expand-clauses clauses)
  (if (null? clauses)
      'false
      (let ((first (car clauses))
            (rest (cdr clauses)))
        (if (cond-else-clause? first)
            (if (null? rest)
                (sequence->exp (cond-actions first))
                (error "ELSE clause isn't last --  COND->IF"
                       clauses))
            (if (arg-pass-syntax? first)
                (make-if (cond-predicate first)
                         (list (arg-pass-action first) (cond-predicate first))
                         (expand-clauses rest))
                (make-if (cond-predicate first)
                         (sequence->exp (cond-actions first))
                         (expand-clauses rest)))))))

(define cond-key 'e)
(define cond-check
  '(cond ((assoc cond-key '((a 1) (b 2) (c 3))) => cadr)
         ((eq? cond-key 'd) => (lambda (arg)
                                 (display "Key is 'd ")
                                 (display "arg is ") (display arg)))
         ((eq? cond-key 'e)
          (display "Key is 'e"))))

(eval (cond->if cond-check) (the-environment))

(define (let-expressions exp)
  (cdr exp))
(define (let-var-names exp)
  (map car (let-expressions exp)))
(define (let-var-exps exp)
  (map cadr (let-expressions exp)))
(define (let-body exp)
  (caddr exp))
(define (let->combination exp)
  (cons
   (make-lambda (let-var-names exp) (list (let-body exp)))
   (let-var-exps exp)))



(define (let*->nested-lets exp)
  (define (transform let-exps let-body)
    (if (null? (cdr let-exps))
        (list 'let (list (car let-exps)) let-body)
        (list 'let (list (car let-exps)) (transform (cdr let-exps) let-body))))
  (transform (let-expressions exp) (let-body exp)))

(define erik '(let* ((x 3)
                     (y (+ x 2))
                     (z (+ x y 5)))
                (* x z)))
(let*->nested-lets erik)

(eval (let*->nested-lets erik) (the-environment))
(apply (eval (operator (let*->nested-lets erik)) (the-environment))
       (list-of-values (operands (let*->nested-lets erik)) (the-environment)))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ;; ...
        ((let*? exp) (eval (let*->nested-lets exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (make-let variables body)
  (list 'let (list variables) body))
(define (named-let-name exp)
  (cadr exp))
(define (named-let? exp)
  (not (pair? (cadr exp))))
(define (named-let-bindings exp)
  (caddr exp))
(define (named-let-body exp)
  (cadddr exp))

(define (named-let->combination exp)
  (make-let (list
             (named-let-name exp)
             (make-lambda (map car (named-let-bindings exp))
                          (list (named-let-body exp))))
            (cons (named-let-name exp)
                  (map cadr (named-let-bindings exp)))))

(define (let->combination exp)
  (if (named-let? exp)
      (named-let->combination exp)
      (cons
       (make-lambda (let-var-names exp) (list (let-body exp)))
       (let-var-exps exp))))

(define fib '(let fib-iter ((a 1)
                            (b 0)
                            (count 10))
               (if (= count 0)
                   b
                   (fib-iter (+ a b) a (- count 1)))))

(let->combination fib)
(named-let? '(let ((a 1))
               (+ a a)))
(named-let-body fib)
(named-let? fib)

(define (fibo n)
  (let ((fib-iter (lambda (a b count)
                    (if (= count 0)
                        b
                        (fib-iter (+ a b) a (- count 1))))))
    (fib-iter 1 0 10)))
(fibo 3)

;; 4.9
(define (while? exp)
  (tagged-list? exp 'while))
(define (while-predicate exp)
  (cadr exp))
(define (while-body exp)
  (cddr exp))

(define (while->if exp)
  (make-if (while-predicate exp)
           (sequence->exp (append (while-body exp) (list exp)))
           '()))

(define meanwhile
  '(while (> x 0)
          (display x)
          (display " is larger than 0")
          (newline)
          (set! x (- x 1))))

(define (until? exp)
  (tagged-list? exp 'until))
(define (until-predicate exp)
  (cadr exp))
(define (until-body exp)
  (cddr exp))

(define (until->if exp)
  (make-if (list 'not (until-predicate exp))
           (sequence->exp (append (until-body exp) (list exp)))
           '()))

(define test-until
  '(until (< x 0)
          (display x)
          (display " is larger than 0")
          (newline)
          (set! x (- x 1))))

(until->if test-until)

(if (not (< x 0))
    (begin
      (display x)
      (display " is larger than 0")
      (newline)
      (set! x (- x 1))
      (until (< x 0)
             (display x)
             (display " is larger than 0")
             (newline)
             (set! x (- x 1))))
    ())


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ;; ..
        ((until? exp) (eval (until->if exp) env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define x 5)


(if (> x 0) (begin (display x) (display " is larger than 0") (newline) (set! x (- x 1)) (while (> x 0) (display x) (display " is larger than 0") (newline) (set! x (- x 1)))) ())




;; 4.1.3

(define (true? x)
  (not (eq? x false)))
(define (false? x)
  (eq? x false))

(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? p)
  (tagged-list? p 'procedure))
(define (procedure-parameters p) (cadr p))
(define (procedure-body p) (caddr p))
(define (procedure-environment p) (cadddr p))

(define (enclosing-environment env) (cdr env))
(define (first-frame env) (car env))
(define the-empty-environment '())

(define (make-frame variables values)
  (cons variables values))
(define (frame-variables frame) (car frame))
(define (frame-values frame) (cdr frame))
(define (add-binding-to-frame! var val frame)
  (set-car! frame (cons var (car frame)))
  (set-cdr! frame (cons val (cdr frame))))

(define (extend-environment vars vals base-env)
  (if (= (length vars) (length vals))
      (cons (make-frame vars vals) base-env)
      (if (< (length vars) (length vals))
          (error "Too many arguments supplied" vars vals)
          (error "Too few arguments supplied" vars vals))))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (car vals))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (define (scan vars vals)
      (cond ((null? vars)
             (add-binding-to-frame! var val frame))
            ((eq? var (car vars))
             (set-car! vals val))
            (else (scan (cdr vars) (cdr vals)))))
    (scan (frame-variables frame)
          (frame-values frame))))


(define (make-frame variables values)
  (if (null? variables)
      '()
      (cons (make-binding (car variables) (car values))
            (make-frame (cdr variables) (cdr values)))))
(define (add-binding-to-frame! var val frame)
  (set! frame (cons (make-binding var val) frame)))
(define (make-binding var val)
  (cons var val))
(define (first-binding frame)
  (car frame))
(define (rest-bindings frame)
  (cdr frame))
(define (binding-var binding)
  (car binding))
(define (binding-val binding)
  (cdr binding))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-env env)))
            ((eq? (binding-var (first-binding frame)) var)
             (binding-val (first-binding frame)))
            (else (scan (rest-bindings frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (define (scan frame)
      (cond ((null? frame)
             (env-loop (enclosing-env env)))
            ((eq? (binding-var (first-binding frame)) var)
             (set-cdr! (first-binding frame) val))
            (else (scan (rest-bindings frame)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((frame (first-frame env)))
          (scan frame))))
  (env-loop env))

(define (define-variable! var val env)
  (define (scan frame)
    (cond ((null? frame)
           (add-binding-to-frame! var val frame))
          ((eq? (binding-var (first-binding frame)) var)
           (set-cdr! (first-binding frame) val))
          (else (scan (rest-bindings frame)))))
  (scan (first-frame env)))


(define the-empty-var '())

(define (find-var var frame)
  (define (scan variables)
    (display "looking ") (display variables) (newline)
    (cond ((null? variables)
           the-empty-var)
          ((eq? (binding-var (first-binding variables))
                var)
           (first-binding variables))
          (else (scan (rest-bindings variables)))))
  (scan frame))

(define (lookup-variable-value var env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((binding (find-var var (first-frame env))))
          (if (eq? binding the-empty-var)
              (env-loop (enclosing-environment env))
              (binding-val binding)))))
  (env-loop env))

(define (set-variable-value! var val env)
  (define (env-loop env)
    (if (eq? env the-empty-environment)
        (error "Unbound variable -- SET!" var)
        (let ((binding (find-var var (first-frame env))))
          (if (eq? binding the-empty-var)
              (env-loop (enclosing-environment env))
              (set-cdr! binding val)))))
  (env-loop env))

(define (define-variable! var val env)
  (let ((frame (first-frame env)))
    (let ((binding (find-var var frame)))
      (if (eq? binding the-empty-var)
          ;; I don't think this is the idea way to do this
          (set-car! env (cons (make-binding var val) frame))
          (set-cdr! binding val)))))

(define (extend-env frame base-env)
  (cons frame base-env))
(define the-frame (make-frame '(a b c d) '(1 2 3 4)))
(define the-env (extend-env the-frame the-empty-environment))


(lookup-variable-value 'b the-env)
(set-variable-value! 'b 20 the-env)
(display the-frame)
(display the-env)
(define-variable! 'e 5 the-env)
(lookup-variable-value 'e the-env)


(define (make-unbound! var env)
  (define (env-loop env)
    (define (scan variables values prev-var prev-val)
      (cond ((null? variables)
             (env-loop (enclosing-environment env)))
            ((eq? (car variables) var)
             (if (null prev-var)
                 (let ((new-frame (make-frame (cdr variables) (cdr values))))
                   (set-car! env new-frame))
                 (begin
                   (set-cdr! prev-var (cdr variables))
                   (set-cdr! prev-val (cdr values)))))
            (else (scan (cdr variables) (cdr values)
                        (car variables) (car values)))))
    (if (not (eq? env the-empty-environment))
        (let ((frame (first-frame env)))
          (scan (frame-variables) (frame-values) '() '()))))
  (env-loop env))

;; 4.1.4

(define (setup-environment)
  (let ((initial-env
         (extend-environment (primitive-procedure-names)
                             (primitive-procedure-objects)
                             the-empty-environment)))
    (define-variable! 'true true initial-env)
    (define-variable! 'false false initial-env)
    initial-env))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        ;; <more primitives>
        ))

(define (primitive-procedure-names)
  (map car primitive-procedures))
(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

;; this assumes a saved reference to the underlying Lisp 'apply' procedure, which is masked by the metacircular 'apply' procedure definition
;; the reference would be saved as ~(define apply-in-underlying-scheme apply)~ being evaluated prior to the definition of 'apply'
(define (apply-primitive-procedure proc args)
  (apply-in-underlying-scheme
   (primtive-implementation proc) args))

;; driver loop
(define input-prompt ";;; M-Eval input:")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output (eval input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))
(define (announce-output string)
  (newline) (display string) (newline))

(define (user-print object)
  (if (compound-procedure? object)
      (display  (list 'compound-procedure
                      (procedure-parameters object)
                      (procedure-body object)
                      '<procedure-env>))
      (display object)))

(define the-global-environment (setup-environment))

(driver-loop)

;; 4.16
(define (lookup-variable-value var env)
  (define (env-loop env)
    (define (scan vars vals)
      (cond ((null? vars)
             (env-loop (enclosing-environment env)))
            ((eq? var (car vars))
             (if (eq? '*unassigned* (car vals))
                 (error "Attempted use of unassigned variable" var)
                 (car vals)))
            (else (scan (cdr vars) (cdr vals)))))
    (if (eq? env the-empty-environment)
        (error "Unbound variable" var)
        (let ((frame (first-frame env)))
          (scan (frame-variables frame)
                (frame-values frame)))))
  (env-loop env))

(define (scan-out-defines procedure-body)
  (let ((let-exp '())
        (set-exp '())
        (body-exp '()))
    (define (check-element element)
      (if (definition? element)
          (let ((var-name (cadr element)))
            (begin
              ;; double quote  *unassigned* so it is a quoted symbol after the 'cons' procedure
              (set! let-exp (cons (list var-name ''*unassigned*) let-exp))
              (set! set-exp (cons (list 'set! var-name (caddr element)) set-exp))))
          (set! body-exp (cons element body-exp))))
    (define (build-let-body)
      (let ((start (list 'let let-exp)))
        (append start set-exp body-exp)))
    (map check-element procedure-body)
    (build-let-body)))

(define (lists-equal? a b)
  (display "checking a: ") (display a) (newline)
  (display "checking b: ") (display b) (newline)
  (cond ((and (null? a) (null? b))
         #t)
        ((and (pair? a) (pair? b))
         (and (lists-equal? (car a) (car b))
              (lists-equal? (cdr a) (cdr b))))
        (else (eq? a b))))

(define test-proc '(lambda (x y)
                     (define v (* x y))
                     (define u (+ x y))
                     (+ u v)))
(define expected-transform '(let ((u '*unassigned*)
                                  (v '*unassigned*))
                              (set! u (+ x y))
                              (set! v (* x y))
                              (+ u v)))
(define (test-scan-out-defines procedure expected)
  ;; (cddr procedure) to extract the body portion of the procedure
  (lists-equal? expected (scan-out-defines (cddr procedure))))
(test-scan-out-defines test-proc expected-transform)

;; 4.16c
(pp (scan-out-defines test-proc))
(define (make-procedure parameters body env)
  (list 'procedure parameters (scan-out-defines body) env))

(define (lambda-params p) (cadr p))
(define (lambda-body p) (cddr p))
(make-procedure (lambda-params test-proc) (lambda-body test-proc) (the-environment))



(lambda (f y0 dt)
  (let ((y '*unassigned*)
        (dy '*unassigned*))
    (let ((a '(integral (delay dy) y0 dt))
          (b '(stream-map f y)))
      (set! y a)
      (set! dy b))
    y))

(lambda (f y0 dt)
 (let ((dy '*unassigned*) (y '*unassigned*))
   (set! dy (stream-map f y))
   (set! y (integral (delay dy) y0 dt))
   y))


(define test-integral '(lambda (f y0 dt)
                         (define y (integral (delay dy) y0 dt))
                         (define dy (stream-map f y))
                         y))
(pp (make-procedure (lambda-params test-integral) (lambda-body test-integral) (the-environment)))

(letrec ((<v1> <e1>) ... (<v2> <e2>))
  <body>)

(let ((v1 undef)
      (v2 undef))
  (set! v1 e1)
  (set! v2 e2)
  body)


(define (letrec-bindings lr)
  (cadr lr))
(define (letrec->let letrec)
  (let ((variables '())
        (procedures '())
        (body (caddr letrec)))
    (define (iter-expressions exps)
      (define (transform-let exp)
        (let ((name (car exp))
              (proc (cadr exp)))
          (set! variables (cons (list name ''*undefined*) variables))
          (set! procedures (cons (list 'set! name proc) procedures))))
      (if (not (null? exps))
          (begin
            (transform-let (car exps))
            (iter-expressions (cdr exps)))))
    (iter-expressions (letrec-bindings letrec))
    (append (list 'let variables) procedures (list body))))

(define test-letrec '(letrec ((even?
                               (lambda (n)
                                 (if (= n 0)
                                     true
                                     (odd? (- n 1)))))
                              (odd?
                               (lambda (n)
                                 (if (= n 0)
                                     false
                                     (even? (- n 1))))))
                       (even? 10)))

;; pretty print transformed expression
(pp (letrec->let test-letrec))

(let ((odd? '*undefined*)
      (even? '*undefined*))
  (set! odd? (lambda (n) (if (= n 0) false (even? (- n 1)))))
  (set! even? (lambda (n) (if (= n 0) true (odd? (- n 1)))))
  (even? 10))

;; 4.1.7 adding analyze -- splitting syntactic analysis from procedure execution
(define (eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

;; looking up a variable must still be done in the execution phase as it requires knowing the environment
(define (analyze-variable exp)
  (lambda (env) (lookup-variable-value exp env)))

(define (analyze-assignment exp)
  (let ((var (assignment-variable exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env)
      'ok)))

(define (analyze-definition exp)
  (let ((var (definition-variable exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (true? (pproc env))
          (cproc env)
          (aproc env)))))

(define (analyze-lambda exp)
  (display "HERE") (newline)
  (let ((vars (lambda-parameters exp))
        (bproc (analyze-sequence (lambda-body exp))))
    (display "p: ") (display vars) (newline)
    (display "b: ") (display bproc) (newline)
    (lambda (env) (make-procedure vars bproc env))))

(define (analyze-sequence exps)
  (display "ANALYZE SEQ") (newline)
  (display "exps: ") (display exps) (newline)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

;; begin DELETE
(analyze-lambda '(lambda (a b) (+ a b)))
(lambda-body '(lambda (a b) (+ a b)))
;; end DELETE



;; illustration of analyze-sequence
;; ;; (a b c d e f)
;; ;; (loop (lambda (env) (a env) (b env))
;; ;;       (c d e f))
;; ;; (loop (lambda (env)
;; ;;         ((lambda (env) (a env) (b env)) env)
;; ;;         (c env))
;; ;;       (d e f))
;; ;; (loop (lambda (env)
;; ;;         ((lambda (env)
;; ;;            ((lambda (env) (a env) (b env)) env)
;; ;;            (c env)) env)
;; ;;         (d env)))

(define (analyze-application exp)
  (display "oper: ") (display (operator exp)) (newline)
  (display "operands: ") (display (operands exp)) (newline)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operands exp))))
    (display "fproc: ") (display fproc) (newline)
    (display "aprocs: ") (display aprocs) (newline)
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc) (aproc env))
                                aprocs)))))

;; execute-application is the analog of apply, but since the procedure has already been analyzed we just call the execution procedure for the body on the extended environment
(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-environment proc))))
        (else
         (error
          "Unknown procedure type -- EXECUTE-APPLICATION"
          proc))))


;; Exercises - 4.22
;; re-using the syntax procedures from exercise 4.6
;; begin - syntax procedures
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))
(define (let? exp)
  (tagged-list? exp 'let))
(define (let-expressions exp)
  (cadr exp))
(define (let-var-names exp)
  (map car (let-expressions exp)))
(define (let-var-exps exp)
  (map cadr (let-expressions exp)))
(define (let-body exp)
  (caddr exp))
(define (let->combination exp)
  (cons
   (make-lambda (let-var-names exp) (list (let-body exp)))
   (let-var-exps exp)))
;; end

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ;; ...
        ((let? exp) (analyze (let->combination exp)))
        ((lambda? exp) (analyze-lambda exp))
        ;; ...
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ;; ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ;; ((if? exp) (analyze-if exp))
        ((let? exp) (analyze (let->combination exp)))
        ((lambda? exp) (analyze-lambda exp))
        ;; ((begin? exp) (analyze-sequence (begin-actions exp)))
        ;; ((cond? exp) (analyze (cond->if exp)))
        ((application? exp) (analyze-application exp))
        (else
         (error "Unknown expression type -- ANALYZE" exp))))

(define check-let '(let ((a 2)
                         (b 3))
                     (+ a b)))

(analyze-application '((lambda (a b) (+ a b)) 2 3))
(let->combination check-let)
((analyze check-let) (the-environment))
(analyze '(lambda (a b) (+ a b)))



(define (other-analyze-sequence exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs)) ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "empty"))
    (lambda (env) (execute-sequence procs env))))

(analyze-sequence '((+ 2 3)))
(other-analyze-sequence '((+ 2 3)))


;; 4.2
(define (unless condition usual-value exceptional-value)
  (if condition exceptional-value usual-value))
(define (factorial n)
  (unless (= n 1)
    (* n (factorial (- n 1)))
    1))

(factorial 5)


(define (unless-condition exp)
  (car exp))
(define (unless-usual-val exp)
  (cadr exp))
(define (unless-exceptional-val exp)
  (caddr exp))
(define (unless->if exp)
  (make-if (unless-condition exp)
           (unless-usual-val exp)
           (unless-exceptional-val exp)))


;; 4.2.2 An Interpreter with Lazy Evaluation

(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ;; change for lazy-evaluation occurrs in the handling of procedure applications
        ((application? exp)
         (apply (actual-value (operator exp) env)
                (operands exp)
                env))
        (else
         (error "Unknown expression type -- EVAL" exp))))

(define (actual-value exp env)
  (force-it (eval exp env)))

(define (apply procedure arguments env)
        ;; primitive-procedures are strict so we evaluate the arguments before passing them in
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure
          procedure
          (list-of-arg-values arguments env))) ; changed
        ;; compound-procedures are non-strict so we delay all the arguments before applying the procedure
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           (list-of-delayed-args arguments env)
           (procedure-environment procedure))))
        (else
         (error
          "Unknown procedure type -- APPLY" procedure))))
;;tmp
(define (eval-sequence exps env)
  (cond ((last-exp? exps) (eval (first-exp exps) env))
        (else (eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))
;;endtmp

(define (list-of-arg-values exps env)
  (if (no-operands? exps)
      '()
      (cons (actual-value (first-operand exps) env)
            (list-of-arg-values (rest-operands exps)
                                env))))

(define (list-of-delayed-args exps env)
  (if (no-operands? exps)
      '()
      (cons (delay-it (first-operand exps) env)
            (list-of-delayed-args (rest-operands exps)
                                  env))))

;; need to change the handling of if so that we use 'actual-value' instead of 'eval' to get the value of the predicate expression
(define (eval-if exp env)
  (if (true? (actual-value (if-predicate exp) env))
      (eval (if-consequent exp) env)
      (eval (if-alternative exp) env)))

;; need to update driver loop to force delayed values if they are propagated backup to the read-eval-print loop.
(define input-prompt ";;; L-Eval input:")
(define  output-prompt ";;; L-Eval value:")

(define (driver-loop)
  (prompt-for-input input-prompt)
  (let ((input (read)))
    (let ((output
           (actual-value input the-global-environment)))
      (announce-output output-prompt)
      (user-print output)))
  (driver-loop))

(define (force-it obj)
  (if (thunk? obj)
      ;; use 'actual-value' instead of 'eval' here in case the value of the expression is a thunk so we continue evaluating until we get to an non-thunk value
      (actual-value (thunk-exp obj) (thunk-env obj))
      obj))

(define (delay-it exp env)
  (list 'thunk exp env))
(define (thunk? obj)
  (tagged-list? obj 'thunk))
(define (thunk-exp thunk) (cadr thunk))
(define (thunk-env thunk) (caddr thunk))

;; memoizing  thunks
(define (evaluated-thunk? obj)
  (tagged-list? obj 'evaluated-thunk))
(define (thunk-value evaluated-thunk) (cadr evaluated-thunk))

(define (force-it obj)
  (cond ((thunk? obj)
         (let ((result (actual-value
                        (thunk-exp obj)
                        (thunk-env obj))))
           (set-car! obj 'evaluated-thunk)
           (set-car! (cdr obj) result) ;; replace exp with its value
           (set-cdr! (cdr obj) '())    ;; forget unneeded env
           result))
        ((evaluated-thunk? obj)
         (thunk-value obj))
        (else obj)))

;; Exercise 4.27
(define count 0)
(define (id x)
  (set! count (+ count 1))
  x)

(define w (id (id 10)))

;; input:
count
;; value
1
;; The value of 'count' here is one because in the definition of 'w' above the outer 'id' procedure is evaluated to proved a value for 'w', resulting in a single evaluation of the '+' procedure to increment count, however the argument to the outer 'id' call, '(id 10)' is delayed as part of the procedure application. When we evaluate a compound procedure as part of the definition of 'w' the outer call is evaluated but the arguments to the outer call are delayed which result in the single increment of 'count'. The reason the outer 'id' is evaluated in the first place is because when evaluating a 'definition' we evaluate the definition-value portion of the define procedure, '(id (id 10))', which evaluates as an 'application?' thereby calling 'eval-sequence' which evaluates the body of the 'id' expression once with a 'thunk' of '(id 10)' representing x. The thunk is not evaluated until it is needed.

;; input
w
;; value
10
;; The value of 'w' after evaluation is the value of the argument 'x'. In the definition of 'w' the inner call to 'id' is passed '10' which is then returned and used as the argument to the outer call to 'id' which also results in '10'. However it does mean the 'id' procedure is evaluated twice incrementing the count, as described below

;; input
count
;; value
2
;; The above evaluation of w results in the forcing of the thunk representing 'x' because of the 'actual-value' call in the driver loop which calls 'force-it' on the expressions. 'force-it' will be called with the thunk object and force the evaluation of the thunk.

(define (determine-proc in)
  (if (= in 0)
      +
      -))


;; Ex 4.29
(define (square x x)
  (* x x))

(square (id 10))
;; 100

count
;; Memoized: 1
;; Non-memoized: 2


(define (cons x y)
  (lambda (m) (m x y)))
(define (car z)
  (z (lambda (p q) p)))
(define (cdr z)
  (z (lambda (p q) q)))


(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))
(define (map proc items)
  (if (null?? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(define (add-lists list1 list2)
  (cond ((null? list1) list2)
        ((null? list2) list1)
        (else (cons (+ (car list1) (car list2))
                    (add-lists (cdr list1) (cdr list2))))))



;; Ex 4.33
;; quote changes
(define (quote-value exp)
  (if (pair? (cadr exp))
      (list-transform (cadr exp))
      (cadr exp)))
(define orig-car car)
(define orig-cdr cdr)
(define (list-transform l)
  (if (null? l)
      '()
      (cons (orig-car l)
            (list-transform (orig-cdr l)))))



(define (prime-sum-pair list1 list2)
  (let ((a (an-element-of list1))
        (b (an-element-of list2)))
    (require (prime? (+ a b)))
    (list a b)))


(define (amb) 'fail)
(define (require p)
  (if (not p) (amb)))


;; an-element-of fails if the list is empty, otherwise it "ambiguosly" returns either the first element of the list or an element chose from the cdr of the list
(define (an-element-of items)
  (require (not (null? items)))
  (amb (car items) (an-element-of (cdr items))))

;; 4.35
(define (an-integer-between low high)
  (require (<= low high))
  (amb low (an-integer-between (+ low 1) high)))

(define (a-pythagorean-triple-between low high)
  (let ((i (an-integer-between low high)))
    (let ((j (an-integer-between i high)))
      (let ((k (an-integer-between j high)))
        (require (= (+ (* i i) (* j j)) (* k k)))
        (list i j k)))))

(define (pythagorean-triples low high)
  (let ((triple (a-pythagorean-triple-between low high)))
    (let ((i (car triple))
          (j (cadr triple))
          (k (caddr triple)))
      (require (= (+ (* i i) (* j j)) (* k k)))
      (amb triple (pythagorean-triples (+ low 1) high)))))


(define (square x) (* x x))

(define (amb) 'fail)
(define (require p)
  (if (not p) (amb)))
(define (an-integer-starting-from low) low)
(define (a-pythagorean-triple-from low)
  (define (k i)
    (if (odd? i)
        (/ (+ (square i) 1) 2)
        (+ (square (/ i 2)) 1)))
  (define (j i)
    (if (odd? i)
        (/ (- (square i) 1) 2)
        (- (square (/ i 2)) 1)))
  (let ((i (an-integer-starting-from low)))
    (require (integer? j))
    (require (integer? k))
    (list i (j i) (k i))))

(a-pythagorean-triple-from 34)

(+ (square 288) (square 34))
(square 290)





(define (multiple-dwelling)
  (let ((baker (amb 1 2 3 4 5))
        (cooper (amb 1 2 3 4 5))
        (fletcher (amb 1 2 3 4 5))
        (miller (amb 1 2 3 4 5))
        (smith (amb 1 2 3 4 5)))
    (require
     (distinct? (list baker cooper fletcher miller smith)))
    (require (not (= baker 5)))
    (require (not (= cooper 1)))
    (require (not (= fletcher 5)))
    (require (not (= fletcher 1)))
    (require (> miller cooper))
    (require (not (= (abs (- smith fletcher)) 1)))
    (require (not (= (abs (- fletcher cooper)) 1)))
    (list (list 'baker baker)
          (list 'cooper cooper)
          (list 'fletcher fletcher)
          (list 'miller miller)
          (list 'smith smith))))

(define (multiple-dwelling)
  (define (rand-of n)
    (+ (random n) 1))
  (let ((baker (rand-of 3))
        (cooper (+ (rand-of 4) 2))
        (fletcher (+ (rand-of 3) 2))
        (miller (rand-of 5))
        (smith (rand-of 5)))
    (if (> miller cooper)
        (if (not (= (abs (- smith fletcher)) 1))
            (if (not (= (abs (- fletcher cooper)) 1))
                (if (distinct? (list baker cooper fletcher miller smith))
                    (list
                     (list 'baker baker)
                     (list 'cooper cooper)
                     (list 'fletcher fletcher)
                     (list 'miller miller)
                     (list 'smith smith))
                    (multiple-dwelling))
                (multiple-dwelling))
            (multiple-dwelling))
        (multiple-dwelling))))

(define (distinct? items)
  (cond ((null? items) true)
        ((null? (cdr items)) true)
        ((member (car items) (cdr items)) false)
        (else (distinct? (cdr items)))))

(multiple-dwelling)


;; parsing natural language
(define nouns '(noun student professor cat class))
(define verbs '(verb studies lectures eats sleeps))
(define articles '(article the a))

(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-word verbs)))


;; Parsing boils down to repeatedly checking that the next unparsed word is a member of the list of words for the required part of speech.
;; *unparsed* is a global that represents the input that is yet to be parsed

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*) (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(parse '(the cat eats))

;; adding to our grammar
(define prepositions '(prep for to in by with))
(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

;; a sentence is a noun phrase followed by a verb phrase, a verb phrase can be either a verb or a verb phrase extended by a prepositional phrase
(define (parse-sentence)
  (list 'sentence
        (parse-noun-phrase)
        (parse-verb-phrase)))
(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb verb-phrase
         (maybe-extend (list 'verb-phrase
                             verb-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))


(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb noun-phrase
         (maybe-extend (list 'noun-phrase
                             noun-phrase
                             (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))


;; basic 'amb' for testing, pick first arg
(define (amb . args)
  (if (null? args)
      'fail
      (car args)))

(define (gen-word word-list)
  (define (choose words)
    (display words) (newline)
    (if (null? words)
        (amb)
        (amb (car words)
             (choose (cdr words)))))
  (let ((words (cdr word-list)))
    (choose words)))

(define (gen-sentence)
  (list 'sentence
        (gen-noun-phrase)
        (gen-verb)))

(define (gen-noun-phrase)
  (list 'noun-phrase
        (gen-word articles)
        (gen-word nouns)))
(define (gen-verb)
  (list 'verb
        (gen-word verbs)))

(gen-word nouns)
(gen-sentence)

(define (analyze-require exp)
  (let ((pproc (analyze (require-predicate exp))))
    (lambda (env succeed fail)
      (pproc env
             (lambda (pred-value fail2)
               (if (not (true? pred-value))
                   (fail2)
                   (succeed 'ok fail2)))
             fail))))
