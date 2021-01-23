;; 3.2.2 The Interpreter for Continuations
(define (evaluate e r k)
  (if (atom? e)
      (cond ((symbol? e) (evaluate-variable e r k))
            (else (evaluate-quote e r k)))
      (case (car e)
        ((quote)  (evaluate-quote (cadr e) r k))
        ((if)     (evaluate-if (cadr e) (caddr e) (cadddr e) r k))
        ((begin)  (evaluate-begin (cdr e) r k))
        ((set!)   (evaluate-set! (cadr e) (caddr e) r k))
        ((lambda) (evaluate-lambda (cadr e) (cddr e) r k))
        ;; 3.4.2
        ((catch)  (evaluate-catch (cadr e) (cddr e) r k))
        ((throw)  (evaluate-throw (cadr e) (caddr e) r k))
        ;; 3.4.3
        ((block)  (evaluate-block (cadr e) (cddr e) r k))
        ((return-from) (evaluate-return-from (cadr e) (caddr e) r k))
        (else     (evaluate-application (car e) (cdr e) r k)))))

;; define-generic, define-method, have the behavior in the text, not the default Guile Scheme behavior
(define-generic (invoke (f) v* r k)
  (wrong "not a function" f r k))
(define-generic (resume (k continuation) v)
  (wrong "Unknown continuation" k))
(define-generic (lookup (r environment) n k)
  (wrong "not an environment" r n k))
(define-generic (update! (r environment) n k v)
  (wrong "not an environment" r n k))

(define-class value Object ())
(define-class environment Object ())
(define-class continuationn Object (k))

;; 3.2.3 Quoting. render the quoted term to the current continuation
(define (evaluate-quote v r k)
  (resume k v))

;; 3.2.4 Alternatives
(define-class if-cont continuation (et ef r))
(define (evaluate-if ec et ef r k)
  (evaluate ec r (make-if-cont k et ef r)))
(define-method (resume (k if-cont) v)
  ;; the resume method is saying:
  ;; evaluate the true expression of the if-cont (~k~ here), et, if v is true, else evaluate the false expression of the if-cont, ef,
  ;; and evaluate them with the environment of the if-cont, and the continuation of the if-cont
  (evaluate (if v (if-cont-et k) (if-cont-ef k))
            (if-cont-r k)
            (if-cont-k k)))

;; 3.2.5 Sequence
(define-class begin-cont continuation (e* r))
(define (evaluate-begin e* r k)
  (if (pair? e*)
      (if (pair? (cdr e*))
          (evaluate (car e*) r (make-begin-cont k e* r))
          (evaluate (car e*) r k))
      (resume k empty-begin-value)))
(define-method (resume (k begin-cont) v)
  (evaluate-begin (cdr (begin-cont-e* k))
                  (begin-cont-r k)
                  (begin-cont-k k)))

;; 3.2.6 Variable Environment
(define-class null-env environment ())
(define-class full-env environment (others name))
(define-class variable-env full-env (value))

(define (evaluate-variable n r k)
  (lookup r n k))
(define-method (lookup (r null-env) n k)
  (wrong "Unknow variable" n r k))
(define-method (lookup (r full-env) n k)
  (lookup (full-env-others r) n k))
(define-method (lookup (r variable-env) n k)
  (if (eqv? n (variable-env-name r))
      (resume k (variable-env-value r))
      (lookup (variable-env-others r) n k)))

(define-class set!-cont continuation (n r))
(define (evaluate-set! n e r k)
  (evaluate e r (make-set!-cont k n r)))
(define-method (resume (k set!-cont) v)
  (update! (set!-cont-r k) (set!-cont-n k) (set!-cont-k k) v))
(define-method (update! (r null-env) n k v)
  (wrong "Unknown variable" n r k))
(define-method (update! (r full-env) n k v)
  (if (eqv? n (variable-env-name r))
      (begin (set-variable-env-value! r v)
             (resume k v))
      (update! (variable-env-others r) n k v)))

;; 3.2.7 Functions
(define-class function value (variables body env))
(define (evaluate-lambda n* e* r k)
  (resume k (make-function n* e* r)))
(define-method (invoke (f function) v* r k)
  (let ((env (extend-env (function-env f)
                         (function-variables f)
                         v*)))
    (evaluate-begin (function-body f) env k)))

(define (extend-env env names values0)
  (cond ((and (pair? names) (pair? values))
         (make-variable-env
          (extend-env env (cdr names) (cdr values))
          (car names)
          (car values)))
        ((and (null? names) (null? values)) env)
        ((symbol? names) (make-variable env env names values))
        (else (wrong "Arity mismatch"))))

(define-class evfun-cont continuation (e* r))
(define-class apply-cont continuation (f r))
(define-class argument-cont continuation (e* r))
(define-class gather-cont continuation (v))
(define (evaluate-application e e* r k)
  (evaluate e r (make-evfun-cont k e* r))) ;; make a new evfun-cont to evaluate the function args and then resume the continuation here of the application of e to the evaluated e* arguments(?)
(define-method (resume (k evfun-cont) f)
  (evaluate-arguments (evfun-cont-e* k)
                      (evfun-cont-r k)
                      (make-apply-cont (evfun-cont-k k)
                                       f
                                       (evfun-cont-r k))))
(define (evaluate-arguments e* r k)
  (if (pair? e*)
      (evaluate (car e*) r (make-argument-cont k e* r))
      (resume k no-more-arguments)))
(define no-more-arguments '())
(define-method (resume (k argument-cont) v)
  (evaluate-arguments (cdr (argument-cont-e* k))
                      (argument-cont-r k)
                      (make-gather-cont (argument-cont-k k) v)))
(define-method (resume (k gather-cont) v*)
  (resume (gather-cont-k k) (cons (gather-cont-v k) v*)))
(define-method (resume (k apply-cont) v)
  (invoke (apply-cont-f k)
          v
          (apply-cont-r k)
          (apply-cont-k k)))

;; 3.3 Initializing the Interpreter
(define-syntax definitial
  (syntax-rules ()
    ((definitial name)
     (definitial name 'void))
    ((definitial name value)
     (begin (set! r.init (make-variable-env r.init 'name value))
            'name))))

(define-class primitive value (name address))
(define-syntax defprimitive
  (syntax-rules ()
    ((defprimitive name value arity)
     (definitial name
       (make-primitive
        'name (lambda (v* r k)
                (if (= arity (length v*))
                    (resume k (apply value v*))
                    (wrong "Incorrect arity" 'name v*))))))))
(define r.init (make-null-env))
(defprimitive cons cons 2)
(defprimitive car car 1)

(define-method (invoke (f primitive) v* r k)
  ((primitive-address f) v* r k))

(define-class bottom-cont continuation (f))
(define-method (resume (k bottom-cont) v)
  ((bottom-cont-f k) v))
(define (chapter3-interpreter)
  (define (toplevel)
    (evaluate (read)
              r.init
              (make-bottom-cont 'void display))
    (toplevel))
  (toplevel))



;; 3.4 Implementing Control Forms
;; call/cc
(definitial call/cc
  (make-primitive
   'call/cc
   (lambda (v* r k)
     (if (= 1 (length v*))
         (invoke (car v*) (list k) r k)
         (wrong "Incorrect arity" 'call/cc v*)))))

;; catch
(define-class catch-cont continuation (body r))
(define-class labeled-cont continuation (tag))
(define (evaluate-catch tag body r k)
  (evaluate tag r (make-catch-cont k body r)))
(define-method (resume (k catch-cont) v)
  (evaluate-begin (catch-cont-body k)
                  (catch-cont-r k)
                  (make-labeled-cont  (catch-cont-k k) v)))

;;  throw
(define-class throw-cont continuation (form r))
(define-class throwing-cont continuation (tag cont))
(define (evaluate-throw tag form r k)
  (evaluate tag r (make-throw-cont k form r)))
(define-method (resume (k throw-cont) tag)
  (catch-lookup k tag k))
(define-method (resume (k throw-cont) tag)
  (catch-lookup k tag k))
(define-generic (catch-lookup (k) tag kk)
  (wrong "Not a continuation" k tag kk))
(define-method (catch-lookup (k continuation) tag kk)
  (catch-lookup (continuation-k k) tag kk))
(define-method (catch-lookup (k bottom-cont) tag kk)
  (wrong "No associated catch" k tag kk))
(define-method (catch-lookup (k labeled-cont) tag kk)
  (if (eqv? tag (labeled-cont-tag k)) ;; comparator
      (evaluate (throw-cont-form kk)
                (throw-cont-r kk)
                (make-throwing-cont kk tag k))
      (catch-lookup (labeled-cont-k k) tag kk)))
(define-method (resume (k throwing-cont) v)
  (resume (throwing-cont-cont k) v))

;; block
(define-class block-cont continuation (label))
(define-class block-env full-env (cont))
(define (evaluate-block label body r k)
  (let ((k (make-block-cont k label)))
    (evaluate-begin body
                    (make-block-env r label k)
                    k)))
(define-method (resume (k block-cont) v)
  (resume (block-cont-k k) v))

;; return-from
(define-class return-from-cont continuation (r label))
(define (evaluate-return-from label form r k)
  (evaluate form r (make-return-from-cont k r label)))
(define-method (resume (k return-from-cont) v)
  (block-lookup (return-from-cont-r k)
                (return-from-cont-label k)
                (return-from-cont-k k)
                v))

(define-generic (block-lookup (r) n k v)
  (wrong "not an environment" r n k v))
(define-method (block-lookup (r block-env) n k v)
  (if (eq? n (block-env-name r))
      (unwind k v (block-env-cont r))
      (block-lookup (blcok-env-others r) n k v)))
(define-method (block-lookup (r full-env) n k v)
  (block-lookup (variable-env-others r) n k v))
(define-method (block-lookup (r null-env) n k v)
  (wrong "Unknown block label" n r k v))
(define-method (resume (k return-from-cont) v)
  (block-lookup (return-from-cont-r k)
                (return-from-cont-label k)
                (return-from-cont-k k)
                v))
(define-generic (unwind (k) v ktarget))
(define-method (unwind (k continuation) v ktarget)
  (if (eq? k ktarget)
      (resume k v)
      (unwind (continuation-k k) v ktarget)))
(define-method (unwind (k bottom-cont) v ktarget)
  (wrong "Obsolete continuation" v))

;; unwind-protect
(define-class unwind-protect-cont continuation (cleanup r))
(define-class protect-return-cont continuation (value))
(define (evaluate-unwind-protect form cleanup r k)
  (evaluate form
            r
            (make-unwind-protect-cont k cleanup r)))
(define-method (resume (k unwind-protect-cont) v)
  (evaluate-begin (unwind-protect-cont-cleanup k)
                  (unwind-protect-cont-r k)
                  (make-protect-return-cont
                   (unwind-protect-cont-k k) v)))
(define-method (resume (k protect-return-cont) v)
  (resume (protect-return-cont-k k) (protect-return-cont-value k)))
