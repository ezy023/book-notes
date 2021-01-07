;; The code here centers around a Lisp2, aka  Common Lisp, interpreter in which the variable environment and function environment are treated separately
(define (f.evaluate e env fenv)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong "Cannot evaluate")))
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (f.evaluate (cadr e) env fenv)
                  (f.evaluate (caddr e) env fenv)
                  (f.evaluate (cadddr e) env fenv)))
        ((begin) (f.eprogn (cdr e) env fenv))
        ((set!) (update! (cadr e)
                         env
                         (f.evaluate (caddr e) env fenv)))
        ((lambda) (f.make-function (cadr e) (cddr e) env fenv))
        ((function) ;; define 'function' to lookup functional value from the function environment.
         (cond ((symbol? (cadr e))
                (lookup (cadr e) fenv))
               (else (wrong "Incorrect function" (cadr e)))))
        ((flet)
         (f.eprogn
          (cddr e)
          env
          (extend fenv
                  (map car (cadr e)) ;; get function name
                  (map (lambda (def)
                         (f.make-function (cadr def) (cddr def) env fenv)) ;; make-function with list-of-vars and body
                       (cadr e)))))
        (else (evaluate-application (car e) ;; the function evaluator
                                    (f.evlis (cdr e) env fenv)
                                    env
                                    fenv)))))

(define (f.evlis exps env fenv)
  (if (pair? exps)
      (cons (f.evaluate (car exps) env fenv)
            (f.evlis (cdr exps) env fenv))
      '()))

(define empty-begin 813)

(define (f.eprogn exps env fenv)
  (if (pair? exps)
      (if (pair? (cdr exps))
          (begin (f.evaluate (car exps) env fenv)
                 (f.eprogn (cdr exps) env fenv))
          (f.evaluate (car exps) env fenv))
      empty-begin))

(define (f.make-function variables body env fenv)
  (lambda (values)
    (f.eprogn body (extend env variables values) fenv)))

(define (f.evaluate-application fn args env fenv)
  (cond ((symbol? fn)
         ((lookup fn fenv) args))
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (f.eprogn (cddr fn) ;; the body of the lambda
                   (extend env (cadr fn) args) ;; extend the variable environment, 'env' with the variables passed as arguments to the application
                   fenv))
        (else (wrong "Incorrect functional term" fn))))

(define (f.lookup id fenv)
  (if (pair? fenv)
      (if (eq? (caar fenv) id)
          (cdar fenv)
          (f.lookup id (cdr fenv)))
      (lambda (values)
        (wrong "No such functional binding" id))))

;; definition of 'funcall'
(define funcall (lambda (args)
                        (if (> (length args) 1)
                            (invoke (car args) (cdr args))
                            (wrong "Incorrect arity" 'funcall))))

(define fenv.global '())
(define-syntax definitial-function
  (syntax-rules ()
    ((definitial-function name)
    (begin (set! fenv.global (cons (cons 'name 'void) fenv.global))
            'name))
    ((definitial-function name value)
     (begin (set! fenv.global (cons (cons 'name value) fenv.global))
            'name))))

;; need to update 'defprimitive' to extend the function environment but not the variable environment
(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial-function name
       (lambda (values)
         (if (= arity (length values))
             (apply value values)
             (wrong "Incorrect arity" (list 'name values))))))))

(defprimitive car car 1)
(defprimitive cons cons 2)

(define (run-lisp2)
  (define (toplevel)
    (display (f.evaluate (read) env.global fenv.global))
    (toplevel))
  (toplevel))


;; Lisp3
;; dynamic varaible interpreter. This interpreter is given a third environment 'denv' that will contain only dynamic variables.
(define (df.evaluate e env fenv denv)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong "Cannot evaluate" e)))
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (df.evaluate (cadr e) env fenv denv)
                  (df.evaluate (caddr e) env fenv denv)
                  (df.evaluate (cadddr e) env fenv denv)))
        ((begin) (df.eprogn (cdr e) env fenv denv))
        ((set!) (update! (cadr e)
                         env
                         (df.evaluate (caddr e) env fenv denv)))
        ((function)
         (cond ((symbol? (cadr e))
                (f.lookup (cadr e) fenv))
               ((and (pair? (cadr e)) (eq? (car (cadr e)) 'lambda))
                (df.make-function
                 (cadr (cadr e)) (cddr (cadr e)) env fenv))
               (else (wrong "Incorrect function" (cadr e)))))
        ((dynamic) (lookup (cadr e) denv))
        ((dynamic-set!)
         (update! (cadr e)
                  denv
                  (df.evaluate (caddr e) env fenv denv)))
        ((dynamic-let)
         (df.eprogn (cddr e)
                    env
                    fenv
                    (extend denv
                            (map car (cadr e))
                            (map (lambda (e)
                                   (df.evaluate e env fenv denv))
                                 (map cadr (cadr e))))))
        (else (df.evaluate-application (car e)
                                       (df.evlis (cdr e) env fenv denv)
                                       env
                                       fenv
                                       denv)))))

(define (df.evaluate-application fn args env fenv denv)
  (cond ((symbol? fn) ((f.lookup fn fenv) args denv))
        ((and (pair? fn) (eq? (car fn) 'lambda))
         (df.eprogn (cddr fn)
                    (extend env (cadr fn) args)
                    fenv
                    denv))
        (else (wrong "Incorrect functional term" fn))))

(define (df.make-function variables body env fenv)
  (lambda (values denv)
    (df.eprogn body (extend env variables values) fenv denv)))
(define (df.eprogn e* env fenv denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (begin (df.evaluate (car e*) env fenv denv)
                 (df.eprogn (cdr e*) env fenv denv))
          (df.evaluate (car e*) env fenv denv))
      empty-begin))

;; dynamic variables without a special form interpreter
(define (dd.evaluate e env denv)
  (if (atom? e)
      (cond ((symbol? e) (lookup e env))
            ((or (number? e) (string? e) (char? e) (boolean? e) (vector? e))
             e)
            (else (wrong "Cannot evaluate" e)))
      (case (car e)
        ((quote) (cadr e))
        ((if) (if (dd.evaluate (cadr e) env denv)
                  (dd.evaluate (caddr e) env denv)
                  (dd.evaluate (cadddr e) env denv)))
        ((begin) (dd.eprogn (cdr e) env denv))
        ((set!) (update! (cadr e)
                         env
                         (dd.evaluate (caddr e) env denv)))
        ((lambda) (dd.make-function (cadr e) (cddr e) env))
        (else (invoke (dd.evaluate (car e) env denv)
                      (dd.evlis (cdr e) env denv)
                      denv)))))
(define (dd.make-function variables body env)
  (lambda (values denv)
    (dd.eprogn body (extend env variables values) denv)))

(define (dd.evlis e* env denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (cons (dd.evaluate (car e*) env denv)
                (dd.evlis (cdr e*) env denv))
          (list (dd.evaluate (car e*) env denv)))
      '()))

(define (dd.eprogn e* env denv)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (begin (dd.evaluate (car e*) env denv)
                 (dd.eprogn (cdr e*) env denv))
          (dd.evaluate (car e*) env denv))
      empty-begin))

;; first function to associate two values: bind-with-dynamic-extent, bind/de
(definitial bind/de
  (lambda (values denv)
    (if (= 3 (length values))
        (let ((tag (car values))
              (value (cadr values))
              (thunk (caddr values)))
          (invoke thunk '() (extend denv (list tag) (list value))))
        (wrong "Incorrect arity" 'bind/de))))

(definitial assoc/de
  (lambda (values current.denv)
    (if (= 2 (length values))
        (let ((tag (car values))
              (default (cadr values)))
          (let look ((denv current.denv))
            (if (pair? denv)
                (if (eqv? tag (caar denv))
                    (cdar denv)
                    (look (cdr denv)))
                (invoke default (list tag) current.denv))))
        (wrong "Incorrect arity" 'assoc/de))))

(define (d.invoke fn args env)
  (if (procedure? fn)
      (fn args env)
      (wrong "Not a function" fn)))
