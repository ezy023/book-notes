;; example
;; (define expt-machine
;;   (make-machine
;;    '(n b product counter)
;;    (list (list '- -) (list '* *) (list '= =))
;;    '(expt-main
;;       (assign counter (reg n))
;;       (assign product (const 1))
;;     expt-loop
;;       (test (op =) (reg counter) (const 0))
;;       (branch (label expt-done))
;;       (assign product (op *) (reg product) (reg b))
;;       (assign counter (op -) (reg counter) (const 1))
;;       (goto expt-loop)
;;       expt-done)))

;; For debugging
(define (print . args)
  (cond ((not (null? args))
         (display (car args))
         (apply print (cdr args))
         (newline))))

(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

;; Ex 5.12 insert into a sorted list
;; for keeping track of instructiions etc
(define (insert-expr exp list)
  (cond ((null? list) (cons exp list))
        ((not (equal? exp (car list)))
         (cons (car list) (insert-expr exp (cdr list))))))

(define (insert-sorted inst list)
  (cond ((null? list) (cons inst '()))
        ((eq? inst (car list)) list)
        ((symbol<? inst (car list))
         (cons inst list))
        (else (cons (car list) (insert-sorted inst (cdr list))))))

;; Register Machine Simulator
(define (make-machine register-names ops controller-text)
  (let ((machine (make-new-machine)))
    (for-each (lambda (register-name)
                ((machine 'allocate-register) register-name))
              register-names)
    ((machine 'install-operations) ops)
    ((machine 'install-instruction-sequence)
     (assemble controller-text machine))
    machine))

;; Registers
(define (make-register name)
  (let ((contents '*unassigned*)
        (value-sources '())
        (trace-enabled #f) ;; Ex 5.18
        )
    (define (add-value-source value-expr)
      (set! value-sources (insert-expr value-expr value-sources)))
    ;; -- Ex 5.18 --
    (define (set-register-contents value)
      (define (print-trace-info)
        (display "Register: ") (display name)
        (display " Old contents: ") (display contents)
        (display " New contents: ") (display value) (newline))
      (if trace-enabled
          (print-trace-info))
      (set! contents value))
    ;; -- end Ex 5.18 --
    (define (dispatch message)
      (cond ((eq? message 'get) contents)
            ((eq? message 'set)
             (lambda (value) (set-register-contents value))) ;; Ex 5.18 changed to call a setter procedure
            ((eq? message 'name) name) ;; Ex 5.12
            ((eq? message 'add-value-source) add-value-source)  ;; Ex 5.12
            ((eq? message 'get-value-sources) value-sources) ;; Ex 5.12
            ((eq? message 'trace-on)
             (begin
               (display "Enabling trace on reg: ") (display name) (newline)
               (set! trace-enabled #t))) ;; Ex 5.18
            ((eq? message 'trace-off) (set! trace-enabled #f)) ;; Ex 5.18
            (else
             (error "Unknown request -- REGISTER" message))))
    dispatch))

(define (get-register-name register)
  (register 'name))
(define (get-contents register)
  (register 'get))
(define (set-contents! register value)
  ((register 'set) value))

;; original stack
;; (define (make-stack)
;;   (let ((s '()))
;;     (define (push x)
;;       (set! s (cons x s)))
;;     (define (pop)
;;       (if (null? s)
;;           (error "Empty stack -- POP")
;;           (let ((top (car s)))
;;             (set! s (cdr s))
;;             top)))
;;     (define (initialize)
;;       (set! s '())
;;       'done)
;;     (define (dispatch message)
;;       (cond ((eq? message 'push) push)
;;             ((eq? message 'pop) (pop))
;;             ((eq? message 'initialize) (initialize))
;;             (else (error "Unknown request -- STACK" message))))
;;     dispatch))

;; instrumented stack, section 5.2.4
(define (make-stack)
  (let ((s '())
        (number-pushes 0)
        (max-depth 0)
        (current-depth 0))
    (define (push x)
      (set! s (cons x s))
      (set! number-pushes (+ 1 number-pushes))
      (set! current-depth (+ 1 current-depth))
      (set! max-depth (max current-depth max-depth)))
    (define (pop)
      (if (null? s)
          (error "Empty stack -- POP")
          (let ((top (car s)))
            (set! s (cdr s))
            top)))
    (define (initialize)
      (set! s '())
      (set! number-pushes 0)
      (set! max-depth 0)
      (set! current-depth 0)
      'done)
    (define (print-statistics)
      (newline)
      (display (list 'total-pushes '= number-pushes
                     'maximum-depth '= max-depth)))
    (define (dispatch message)
      (cond ((eq? message 'push) push)
            ((eq? message 'pop) (pop))
            ((eq? message 'initialize) (initialize))
            ((eq? message 'print-statistics)
             (print-statistics))
            (else
             (error "Unknown request -- STACK" message))))
    dispatch))


(define (pop stack)
  (stack 'pop))

(define (push stack value)
  ((stack 'push) value))

;; The basic machine

(define (start machine)
  (machine 'start))

(define (get-register-contents machine register-name)
  (get-contents (get-register machine register-name)))

(define (set-register-contents! machine register-name value)
  (set-contents! (get-register machine register-name) value)
  'done)

(define (get-register machine reg-name)
  ((machine 'get-register) reg-name))

;; -- Ex 5.19 --
(define (set-breakpoint machine label n)
  ((machine 'set-breakpoint) label n))

(define (proceed-machine machine)
  ((machine 'proceed-from-breakpoint)))

;; (define (cancel-breakpoint machine label n))

;; (define (cancel-all-breakpoints machine))
;; -- end Ex 5.19 --

(define (make-new-machine)
  (let ((pc (make-register 'pc))
        (flag (make-register 'flag))
        (stack (make-stack))
        (the-instruction-sequence '())
        (seen-instructions '())       ;; ex 5.12
        (goto-registers '())          ;; ex 5.12
        (save-restore-registers '())  ;; ex 5.12
        (executed-insts-count 0)      ;; ex 5.15
        (trace-enabled #f)            ;; ex 5.16
        (trace-count 0)               ;; ex 5.17 to delineate each instruction sequence printed
        (prev-executed-insts '())     ;; ex 5.17
        (breakpoints '())             ;; ex 5.19
        (next-breakpoint '())         ;; ex 5.19
        )
    (let ((the-ops
           (list (list 'initialize-stack
                       (lambda () (stack 'initialize)))
                 ;; added to measure stack performance, section 5.2.4
                 (list 'print-stack-statistics
                       (lambda () (stack 'print-statistics)))))
          (register-table
           (list (list 'pc pc) (list 'flag flag))))
      (define (allocate-register name)
        (if (assoc name register-table)
            (error "Multiply defined register: " name)
            (set! register-table
                  (cons (list name (make-register name))
                        register-table)))
        'register-allocated)
      (define (lookup-register name)
        (let ((val (assoc name register-table)))
          (if val
              (cadr val)
              (error "Unknown register: " name))))
      (define (execute)
        (let ((insts (get-contents pc)))
          (if (null? insts)
              'done
              (cond ((should-break insts)
                     (print "instruction: " insts)
                     'breakpoint-reached)
                    (else
                     (begin
                     (set! executed-insts-count (+ executed-insts-count 1)) ;; ex 5.15
                     (print-instruction (car insts)) ;; ex 5.16
                     (set! prev-executed-insts (append prev-executed-insts (list (car insts)))) ;; ex 5.17
                     ((instruction-execution-proc (car insts)))
                     (execute)))))))
      ;; -- Ex 5.19 --
      (define (should-break insts)
        (check-breakpoints (car insts))
        (if (null? insts)
            #f
            (if (not (null? next-breakpoint))
                (let ((next-offset (breakpoint-offset next-breakpoint)))
                  (if (<= next-offset 1)
                      #t
                      (let ((new-offset (- (breakpoint-offset next-breakpoint) 1)))
                        (display "Setting breakpoint") (newline)
                        (set! next-breakpoint (cons (breakpoint-label next-breakpoint)
                                                    new-offset))
                        #f)))
                #f)))
      ;; -- end Ex 5.19 --
      ;; -- Ex 5.12 --
      (define (add-instruction inst)
        (set! seen-instructions (insert-sorted inst seen-instructions)))
      (define (add-goto-reg reg)
        (set! goto-registers (insert-sorted reg goto-registers)))
      (define (add-save-restore-reg reg)
        (set! save-restore-registers (insert-sorted reg save-restore-registers)))
      (define (register-value-sources)
        (map (lambda (reg-table-entry)
                    (let ((register (cadr reg-table-entry))
                          (reg-label (car reg-table-entry)))
                      (cons reg-label (register 'get-value-sources))))
                  register-table))
      ;; -- end Ex 5.12 procedures --
      ;; -- Ex 5.16 --
      (define (print-instruction inst)
        ;; -- Ex 5.17 Added to delineate different instruction sequences printed --
        (define (show-trace-number)
          (newline)
          (display "TRACE ") (display trace-count) (newline)
          (display "-------") (newline))
        (cond (trace-enabled
               (set! trace-count (+ trace-count 1))
               (show-trace-number)
               ;; (for-each (lambda (prev-inst-label)
               ;;             (display prev-inst-label)
               ;;             (newline))
               ;;           (map car prev-executed-insts))
               (display (car inst))
               (newline))))
      ;; -- end Ex 5.16 --
      ;; -- Ex 5.19 --
      (define (add-breakpoint label offset)
        ;; breakpoints stored as a table of breakpoint labels to the corresponding offset
        (display "Adding breakpoint: ") (display label) (display " ") (display offset) (newline)
        (set! breakpoints (cons (cons label offset) breakpoints)))
      (define (breakpoint-label bkpt) (car bkpt))
      (define (breakpoint-offset bkpt) (cdr bkpt))
      (define (proceed-machine)
        (set! next-breakpoint '())
        (execute))
      (define (check-breakpoints inst)
        (define (iter bkpts)
          (cond ((null? bkpts) 'done)
                ((equal? (car inst) (breakpoint-label (car bkpts)))
                 ;; this potentially will clobber an existing breakpoint and could be made smarter
                 (set! next-breakpoint (car bkpts)))
                (else
                 (iter (cdr bkpts)))))
        (iter breakpoints))
      ;; -- end Ex 5.19 --
      (define (dispatch message)
        (cond ((eq? message 'start)
               (set-contents! pc the-instruction-sequence)
               (execute))
              ((eq? message 'install-instruction-sequence)
               (lambda (seq) (set! the-instruction-sequence seq)))
              ((eq? message 'allocate-register) allocate-register)
              ((eq? message 'get-register) lookup-register)
              ((eq? message 'install-operations)
               (lambda (ops) (set! the-ops (append the-ops ops))))
              ((eq? message 'stack) stack)
              ((eq? message 'operations) the-ops)
              ;; -- Ex 5.12
              ((eq? message 'add-inst) add-instruction)
              ((eq? message 'add-goto-reg) add-goto-reg)
              ((eq? message 'add-save-restore) add-save-restore-reg)
              ((eq? message 'seen-instructions) seen-instructions)
              ((eq? message 'goto-registers) goto-registers)
              ((eq? message 'save-restore-registers) save-restore-registers)
              ((eq? message 'register-value-sources) (register-value-sources))
              ;; end Ex 5.12 --
              ((eq? message 'executed-instruction-count) executed-insts-count) ;; ex 5.15
              ((eq? message 'trace-on) ;; ex 5.16
               (set! trace-enabled #t))
              ((eq? message 'trace-off) ;; ex 5.16
               (set! trace-enabled #f))
              ((eq? message 'trace-register-on) ;; ex 5.18
               (lambda (register) (register 'trace-on)))
              ((eq? message 'trace-register-off) ;; ex 5.18
               (lambda (register) (register 'trace-off)))
              ;; -- Ex 5.19 --
              ((eq? message 'set-breakpoint) add-breakpoint)
              ((eq? message 'proceed-from-breakpoint) (proceed-machine))
              ;; -- Ex 5.19
              (else (error "Unknown request -- MACHINE" message))))
      dispatch)))

;; The Assembler
(define (assemble controller-text machine)
  (extract-labels controller-text
                  (lambda (insts labels)
                    (update-insts! insts labels machine)
                    insts)))

(define (extract-labels text receive-proc)
  (if (null? text)
      (receive-proc '() '())
      (extract-labels (cdr text)
                      (lambda (insts labels)
                        (let ((next-inst (car text)))
                          (if (symbol? next-inst)
                              (receive-proc insts
                                            (cons (make-label-entry next-inst
                                                                    insts)
                                                  labels))
                              (receive-proc (cons (make-instruction next-inst)
                                                  insts)
                                            labels)))))))

(define (update-insts! insts labels machine)
  (let ((pc (get-register machine 'pc))
        (flag (get-register machine 'flag))
        (stack (machine 'stack))
        (ops (machine 'operations)))
    (for-each
     (lambda (inst)
       (set-instruction-execution-proc!
        inst
        (make-execution-procedure
         (instruction-text inst) labels machine
         pc flag stack ops)))
     insts)))

(define (make-instruction text)
  (cons text '()))

(define (instruction-text inst)
  (car inst))

(define (instruction-execution-proc inst)
  (cdr inst))

(define (set-instruction-execution-proc! inst proc)
  (set-cdr! inst proc))

(define (make-label-entry label-name insts)
  (cons label-name insts))

(define (lookup-label labels label-name)
  (let ((val (assoc label-name labels)))
    (if val
        (cdr val)
        (error "Undefined label -- ASSEMBLE" label-name))))

;; Generating Execution Procedures for Instructions
(define (make-execution-procedure inst labels machine pc flag stack ops)
  ((machine 'add-inst) (car inst)) ;; Ex 5.12
  (cond ((eq? (car inst) 'assign)
         (make-assign inst machine labels ops pc))
        ((eq? (car inst) 'test)
         (make-test inst machine labels ops flag pc))
        ((eq? (car inst) 'branch)
         (make-branch inst machine labels flag pc))
        ((eq? (car inst) 'goto)
         (make-goto inst machine labels pc))
        ((eq? (car inst) 'save)
         (make-save inst machine stack pc))
        ((eq? (car inst) 'restore)
         (make-restore inst machine stack pc))
        ((eq? (car inst) 'perform)
         (make-perform inst machine labels ops pc))
        (else (error "Unknown instruction type -- ASSEMBLE" inst))))

;; Assign
(define (make-assign inst machine labels operations pc)
  (let ((target
         (get-register machine (assign-reg-name inst)))
        (value-exp (assign-value-exp inst)))
    ((target 'add-value-source) value-exp) ;; Ex 5.12
    (let ((value-proc
           (if (operation-exp? value-exp)
               (make-operation-exp
                value-exp machine labels operations)
               (make-primitive-exp
                (car value-exp) machine labels))))
      (lambda ()                    ; execution procedure for 'assign'
        (let ((val-result (value-proc)))
          (set-contents! target val-result)
          (advance-pc pc))))))

(define (assign-reg-name assign-instruction)
  (cadr assign-instruction))

(define (assign-value-exp assign-instruction)
  (cddr assign-instruction))

(define (advance-pc pc)
  (set-contents! pc (cdr (get-contents pc))))


;; Test, branch, and goto
(define (make-test inst machine labels operations flag pc)
  (let ((condition (test-condition inst)))
    (if (operation-exp? condition)
        (let ((condition-proc
               (make-operation-exp
                condition machine labels operations)))
          (lambda ()
            (set-contents! flag (condition-proc))
            (advance-pc pc)))
        (error "Bad TEST instruction -- ASSEMBLE" inst))))

(define (test-condition test-instruction)
  (cdr test-instruction))

(define (make-branch inst machine labels flag pc)
  (let ((dest (branch-dest inst)))
    (if (label-exp? dest)
        (let ((insts
               (lookup-label labels (label-exp-label dest))))
          (lambda ()
            (if (get-contents flag)
                (set-contents! pc insts)
                (advance-pc pc))))
        (error "Bad BRANCH instruction -- ASSEMBLE" inst))))

(define (branch-dest branch-instruction)
  (cadr branch-instruction))

(define (make-goto inst machine labels pc)
  (let ((dest (goto-dest inst)))
    (cond ((label-exp? dest)
           (let ((insts
                  (lookup-label labels
                                (label-exp-label dest))))
             (lambda () (set-contents! pc insts))))
          ((register-exp? dest)
           (let ((reg
                  (get-register machine
                                (register-exp-reg dest))))
             ((machine 'add-goto-reg) (get-register-name reg)) ;; Ex 5.12
             (lambda ()
               (set-contents! pc (get-contents reg)))))
          (else (error "Bad GOTO instruction -- ASSEMBLE"
                       inst)))))

(define (goto-dest goto-instruction)
  (cadr goto-instruction))

;; Other instructions
(define (make-save inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    ((machine 'add-save-restore) (get-register-name reg)) ;; Ex 5.12
    (lambda ()
      (push stack (get-contents reg))
      (advance-pc pc))))

(define (make-restore inst machine stack pc)
  (let ((reg (get-register machine
                           (stack-inst-reg-name inst))))
    ((machine 'add-save-restore) (get-register-name reg)) ;; Ex 5.12
    (lambda ()
      (set-contents! reg (pop stack))
      (advance-pc pc))))

(define (stack-inst-reg-name stack-instruction)
  (cadr stack-instruction))

(define (make-perform inst machine labels operations pc)
  (let ((action (perform-action inst)))
    (if (operation-exp? action)
        (let ((action-proc
               (make-operation-exp
                action machine labels operation)))
          (lambda ()
            (action-proc)
            (advance-pc pc)))
        (error "Bad PERFORM instruction -- ASSEMBLE" inst))))

(define (perform-action inst) (cdr inst))

;; Execution procedures for subexpressions
(define (make-primitive-exp exp machine labels)
  (cond ((constant-exp? exp)
         (let ((c (constant-exp-value exp)))
           (lambda () c)))
        ((label-exp? exp)
         (let ((insts
                (lookup-label labels
                              (label-exp-label exp))))
           (lambda () insts)))
        ((register-exp? exp)
         (let ((r (get-register machine
                                (register-exp-reg exp))))
           (lambda () (get-contents r))))
        (else
         (error "Unknown expression type -- ASSEMBLE" exp))))

(define (register-exp? exp) (tagged-list? exp 'reg))
(define (register-exp-reg exp) (cadr exp))
(define (constant-exp? exp) (tagged-list? exp 'const))
(define (constant-exp-value exp) (cadr exp))
(define (label-exp? exp) (tagged-list? exp 'label))
(define (label-exp-label exp) (cadr exp))

(define (make-operation-exp exp machine labels operations)
  (let ((op (lookup-prim (operation-exp-op exp) operations))
        (aprocs
         (map (lambda (e)
                (make-primitive-exp e machine labels))
              (operation-exp-operands exp))))
    (lambda ()
      (apply op (map (lambda (p) (p)) aprocs)))))

(define (operation-exp? exp)
  (and (pair? exp) (tagged-list? (car exp) 'op)))

(define (operation-exp-op operation-exp)
  (cadr (car operation-exp)))

(define (operation-exp-operands operation-exp)
  (cdr operation-exp))

(define (lookup-prim symbol operations)
  (let ((val (assoc symbol operations)))
    (if val
        (cadr val)
        (error "Unknown operation -- ASSEMBLE" symbol))))

;; -- Exercise 5.12 --
(define fib-controller
  '(controller
      (assign n (const 8))
      (assign continue (label fib-done))
    fib-loop
      (test (op <) (reg n) (const 2))
      (branch (label immediate-answer))
      (save continue)
      (assign continue (label afterfib-n-1))
      (save n)
      (assign n (op -) (reg n) (const 1))
      (goto (label fib-loop))
    afterfib-n-1
      (restore n)
      (restore continue)
      (assign n (op -) (reg n) (const 2))
      (save continue)
      (assign continue (label afterfib-n-2))
      (save val)
      (goto (label fib-loop))
    afterfib-n-2
      (assign n (reg val))
      (restore val)
      (restore continue)
      (assign val (op +) (reg val) (reg n))
      (goto (reg continue))
    immediate-answer
      (assign val (reg n))
      (goto (reg continue))
    fib-done))

(define fib-machine
  (make-machine
   '(n continue val)
   (list (list '- -) (list '+ +) (list '< <))
   fib-controller))

;; (define gcd-machine
;;   (make-machine
;;    '(a b t)
;;    (list (list 'rem remainder) (list '= =))
;;    '(test-b
;;      (test (op =) (reg b) (const 0))
;;      (branch (label gcd-done))
;;      (assign t (op rem) (reg a) (reg b))
;;      (assign a (reg b))
;;      (assign b (reg t))
;;      (goto (label test-b))
;;      gcd-done)))

;; (display (fib-machine 'seen-instructions))
;; (display (fib-machine 'goto-registers))
;; (display (fib-machine 'save-restore-registers))
;; (display (fib-machine 'register-value-sources))

;; -- end Exercise 5.12 --
(fib-machine 'trace-on)
((fib-machine 'trace-register-on) ((fib-machine 'get-register) 'n)) ;; Ex 5.18
;; (set-breakpoint fib-machine '(branch (label immediate-answer)) 3) ;; Ex 5.19
(fib-machine 'start)
(get-register-contents fib-machine 'n)
(get-register-contents fib-machine 'continue)
(get-register-contents fib-machine 'val)
(get-register-contents fib-machine 'pc)
;; (proceed-machine fib-machine)
;; (display (fib-machine 'executed-instruction-count))
