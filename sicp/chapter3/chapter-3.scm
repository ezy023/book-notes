(define balance 100)

(define (withdraw amount)
  (if (>= balance amount)
      (begin (set! balance (- balance amount))
             balance)
      "Insufficient funds"))


(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin (set! balance (- balance amount))
                 balance)
          "Insufficient funds"))))

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds")))

(define W1 (make-withdraw 100))
(define W2 (make-withdraw 100))

(W1 50)
(W2 70)
(W2 40)
(W1 40)

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknown request -- MAKE-ACCOUNT"
                       m))))
  dispatch)

(define acc (make-account 100))

((acc 'deposit) 10)



(define A (make-accumulator 5))
(A 10)
(A 10)

(define (make-monitored f)
  (let ((count 0))
    (define (how-many-calls?)
      count)
    (define (reset-count)
      (set! count 0))
    (define (dispatch m)
      (cond ((eq? m 'how-many-calls?) (how-many-calls?))
            ((eq? m 'reset-count) (reset-count))
            (else
             (begin (set! count (+ count 1))
                    (f m)))))
    dispatch))

(define (my-display s) (display s))
(define p (make-monitored my-display))

(p "test")
(p 'how-many-calls?)
(p 'reset-count)


(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw passwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password")))
  dispatch)

(define acc (make-account 100 'some-password))
((acc 'some-password 'withdraw) 40)
((acc 'some-other-password 'deposit) 50)


(define (make-account balance passwd)
  (let ((attempts 7))
    (define (withdraw amount)
     (if (>= balance amount)
         (begin (set! balance (- balance amount))
                balance)
         "Insufficient funds"))
   (define (deposit amount)
     (set! balance (+ balance amount))
     balance)
   (define (call-the-cops)
     (error "Too many invalid attempts, calling the cops"))
   (define (dispatch pw m)
     (if (eq? pw passwd)
         (cond ((eq? m 'withdraw) withdraw)
               ((eq? m 'deposit) deposit)
               (else (error "Unknown request -- MAKE-ACCOUNT"
                            m)))
         (begin
           (if (= attempts 0)
               (call-the-cops)
               (set! attempts (- attempts 1)))
           (error "Incorrect password"))))
   dispatch))

(define acc (make-account 100 'some-password))
((acc 'wrong-password 'withdraw) 90)


(define (rand-update x)
  (modulo (+ 31 (* 29 x)) 11))
(define random-init 23)
(define my-rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))
(define (cesaro-test)
  (= (gcd (my-rand) (my-rand)) 1))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (display trials-passed) (display " ") (display trials)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

(define (estimate-pi trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-remaining 1)))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))


(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))
(define (predicate x y r)
  (lambda (xg yg)
    (<= (square r)
        (+ (square (- xg x))
           (square (- yg y))))))
(define (area-of-square x1 x2 y1 y2)
  (* (- x2 x1)
     (- y2 y1)))
(define (estimate-integral P x1 x2 y1 y2 trials)
  (* (monte-carlo trials (lambda () (P (random-in-range x1 x2)
                                       (random-in-range y1 y2))))
     (area-of-square x1 x2 y1 y2)))
(estimate-integral (predicate 5 7 3) 2 8 4 10 1000)


(define my-rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))

(define my-rand
  (let ((initial random-init))
    (let ((x initial))
      (define (generate)
        (begin (set! x (rand-update x))
               x))
      (define (reset val)
        (set! x val))
      (define (dispatch m)
        (cond ((eq? m 'generate)
               (generate))
              ((eq? m 'reset)
               reset)
              (else
               (error "Unknown procedure -- MY-RAND"))))
      dispatch)))

((my-rand 'reset) 8)
(my-rand 'generate)



(define (make-account balance passwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pw m)
    (if (eq? pw passwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT"
                           m)))
        (error "Incorrect password -- MAKE-ACCOUNT" (list pw))))
  dispatch)

(define (make-joint account password new-password)
  (define (dispatch pw m)
    (if (eq? pw new-password)
        (account password m)
        (error "Incorrect password -- MAKE-JOINT" pw)))
  dispatch)

(define peter-acc (make-account 100 'open-sesame))
(define paul-acc (make-joint peter-acc 'open-sesame 'rosebud))

((peter-acc 'open-sesame 'deposit) 100)
((paul-acc 'rosebud 'withdraw) 150)
((paul-acc 'rosebud 'deposit) 200)

(define f
  (let ((prev (- 1)))
    (lambda (x)
      (display "prev ") (display prev) (display " x ") (display x) (newline)
      (if (= prev -1)
          (begin
            (set! prev x)
            (display "2prev ") (display prev) (display " 2x ") (display x) (newline)
            x)
          (if (= prev 0)
              0
              0)))))

(define g
  (let ((prev (- 1)))
    (lambda (x)
      (display "prev ") (display prev) (display " x ") (display x) (newline)
      (if (= prev -1)
          (begin
            (set! prev x)
            (display "2prev ") (display prev) (display " 2x ") (display x) (newline)
            x)
          (if (= prev 0)
              0
              0)))))

(+ (f 0) (f 1))
(+ (g 1) (g 0))



(define (my-append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define x (list 'a 'b))
(define y (list 'c 'd))
(define z (my-append! x y))

(cdr x)
(define w (append! x y))

(cdr x)


(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

(define v (list 'a 'b 'c 'd))
(define w (mystery v))
(display w)
w
v
(display v)


(define x (list 'a 'b))
(define z1 (cons x x))

(define z2 (cons '(a b) '(a b)))
(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

(set-to-wow! z1)
(set-to-wow! z2)

(define (count-pairs x)
  (let ((seen '()))
    (define (count y)
      (cond ((not (pair? y)) 0)
            ((memq y seen) 0)
            (else
             (set! seen (cons y seen))
             (+ (count (car y))
                (count (cdr y))
                1))))
    (count x)))




(define z1 '(a b c))
(count-pairs z1)

(define z2 '((a b) c d))
(count-pairs z2)

(define z3 '((a b) (c d)))
(count-pairs z3)

(define z4 (cons (cons 'a 'b) (cons 'c 'd)))
(count-pairs z4)

(define z5 (cons (cons (cons 'a 'b) 'c)
                 (cons 'd 'e)))
(count-pairs z5)


(define (contains? x l)
  (cond ((null? l) false)
        ((not (pair? (car l)))
         (if (eq? x (car l))
             true
             (contains? x (cdr l))))
        ((pair? (car l)))
        (else
         (or
          (contains? x (car l))
          (contains? x (cdr l))))))

(contains? 'a (list (list 'a 'b) 'c 'd))
(contains? 'b '(a))
(contains? 'a '(((a) b) c d))
(contains? 'a '((b c) (d a)))
(contains? 'a '((c) (b d (e (a)))))

(define (cycle? l)
  (if (null? l)
      false
      (if (contains? (car l) (cdr l))
          true
          (cycle? (cdr l)))))

(define x '(a b c d e a))
(cycle? x)
(set-cdr!)

(define (cycle? l)
  (let ((seen '()))
    (define (check-cycle segment)
      (cond ((null? segment) false)
            ((memq (car segment) seen) true)
            (else
             (begin
               (set! seen (cons (car segment) seen))
               (check-cycle (cdr segment))))))
    (check-cycle l)))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)
(define z (make-cycle (list 'a 'b 'c)))

(define (cycle? l)
  (define (check-cdrs f s)
    (cond ((null? s) #f)
          ((null? f) #f)
          ((eq? (car f) (car s))
           #t)
          (else
           (if (null? (cdr s))
               #f
               (check-cdrs (cdr f) (cddr s))))))
  (check-cdrs (cdr l) (cddr l)))

(cycle? (list 'a 'b 'c 'd 'e))
(cycle? (circular-list 'a 'b 'c 'd 'e))


;; Queues
;; A queue can be represented by a pair consisting of a pointer to the front element and a pointer to the last element in a list
(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue"))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))


(define (print-queue queue)
  (display (front-ptr queue)))

(define q1 (make-queue))
(insert-queue! q1 'a)
(insert-queue! q1 'b)
(insert-queue! q1 'c)
(display q1)
(print-queue q1)
(delete-queue! q1)
(display q1)
(print-queue q1)


(define (new-make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    (define (insert-queue! item)
      (let ((new-entry (cons item '())))
        (if (empty-queue?)
            (begin
              (set! front-ptr new-entry)
              (set! rear-ptr new-entry)
              front-ptr)
            (begin
              (set-cdr! rear-ptr new-entry)
              (set! rear-ptr new-entry)
              front-ptr))))
    (define (delete-queue!)
      (if (empty-queue?)
          (error "DELETE called with an empty queue")
          (begin
            (set! front-ptr (cdr front-ptr))
            front-ptr)))
    (define (print-queue)
      (display front-ptr)
      (newline)
      (display rear-ptr)
      (newline)
      (display (cons front-ptr rear-ptr)))
    (define (dispatch m)
      (cond ((eq? m 'empty-queue?) empty-queue?)
            ((eq? m 'insert-queue!) insert-queue!)
            ((eq? m 'delete-queue!) delete-queue!)
            ((eq? m 'print-queue) print-queue)
            (else (error "No method defined for queue -- DISPATCH " m))))
    dispatch))

(define q2 (new-make-queue))
((q2 'print-queue))
((q2 'insert-queue!) 'a)
((q2 'insert-queue!) 'b)
((q2 'insert-queue!) 'c)
((q2 'delete-queue!))


(define (make-deque)
  (cons '() '()))
(define (front-deque d)
  (car d))
(define (rear-deque d)
  (cdr d))
(define (set-front-deque! d v)
  (set-car! d v))
(define (set-rear-deque! d v)
  (set-cdr! d v))
(define (empty-deque? d)
  (and (null? (front-deque d))
       (null? (rear-deque d))))
(define (front-insert-deque! d item)
  (let ((new-pair (cons item '())))
    (set-cdr! new-pair (front-deque d))
    (set-front-deque! d new-pair)
    d))
(define (rear-insert-deque! d item)
  (let ((new-pair (cons item '())))
    (if (empty-deque? d)
       (begin
         (set-front-deque! d new-pair)
         (set-rear-deque! d new-pair)
         d)
       (begin
         (set-cdr! (rear-deque d) new-pair)
         (set-rear-deque! d new-pair)
         d))))
(define (front-delete-deque! d)
  (set-front-deque! d (cdr (front-deque d))))
(define (rear-delete-deque! d)
  (define))

(define d1 (make-deque))
(rear-insert-deque! d1 'a)  ; (a)
(rear-insert-deque! d1 'b)  ; (a b)
(front-insert-deque! d1 'c) ; (c a b)
(rear-delete-deque! d1)     ; (c a)
(front-delete-deque! d1)    ; (a)

(print-deque d1)

;; Deque

;;; Deque Entry
(define (make-deque-entry val)
  ;; entry is a pair of (val, prev-pointer)
  (cons val '()))
(define (deque-entry-prev entry)
  (cdr entry))
(define (deque-entry-value entry)
  (car entry))
(define (set-deque-entry-prev! entry v)
  (set-cdr! entry v))

;;; Deque impl
(define (make-deque)
  (cons '() '()))
(define (front-deque d)
  (car d))
(define (rear-deque d)
  (cdr d))
(define (set-front-deque! d v)
  (set-car! d v))
(define (set-rear-deque! d v)
  (set-cdr! d v))
(define (empty-deque? d)
  (and (null? (front-deque d))
       (null? (rear-deque d))))
(define (deque-pair-entry pair)
  (car pair))
(define (front-insert-deque! d item)
  (let ((new-entry (make-deque-entry item))
        (head (front-deque d)))
    (let ((new-pair (cons new-entry '())))
      (set-deque-entry-prev! (deque-pair-entry head) new-entry)
      ;; could (cons new-entry head) instead of set-cdr! here
      (set-cdr! new-pair head)
      (set-front-deque! d new-pair))))
(define (rear-insert-deque! d item)
  (let ((new-entry (make-deque-entry item)))
    (let ((new-pair (cons new-entry '())))
      (if (empty-deque? d)
         (begin
           (set-front-deque! d new-pair)
           (set-rear-deque! d new-pair))
         (begin
           (let ((tail (rear-deque d)))
             (set-deque-entry-prev! new-entry tail)
             (set-cdr! tail new-pair)
             (set-rear-deque! d new-pair)))))))
(define (front-delete-deque! d)
  (set-front-deque! d (cdr (front-deque d))))
(define (rear-delete-deque! d)
  (let ((tail (rear-deque d)))
    (let ((tail-prev (deque-entry-prev (deque-pair-entry tail))))
      (set-cdr! tail-prev '())
      (set-rear-deque! d tail-prev))))
(define (print-deque d)
  (map deque-entry-value (front-deque d)))


(define d1 (make-deque))
(rear-insert-deque! d1 'a)  ; (a)
(rear-insert-deque! d1 'b)  ; (a b)
(front-insert-deque! d1 'c) ; (c a b)
(rear-delete-deque! d1)     ; (c a)
(front-delete-deque! d1)    ; (a)

(print-deque d1)

;; Tables
(define (lookup key table)
  ;; (cdr table) because the table is represented by a 'headed list' and the first record is the dummy record
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))

(define (assoc key records)
  (cond ((null? records) false)
        ;; a record is a pair with the car cell pointing to a pair of (key . val) and the cdr cell pointing to the next record
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  ;; again, (cdr table) because the first record is the dummy record
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))))))

(defin (make-table)
  (list '*table*))

(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table))))))


(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (make-table->same-key same-key?)
  (let ((local-table (list '*table*)))
    (define (lookup key)
      (let ((record (same-key? key (cdr local-table))))
        (if record
            (cdr record)
            false)))
    (define (insert! key value)
      (let ((record (same-key? key (cdr local-table))))
        (if record
            (set-cdr! record value)
            (set-cdr! local-table
                      (cons (cons key value)
                            (cdr local-table))))))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t1 (make-table->same-key assoc))
((t1 'lookup-proc) "first")
((t1 'insert-proc!) "first" 1)
((t1 'insert-proc!) "second" 2)
((t1 'lookup-proc) "first")
((t1 'lookup-proc) "second")

(define (assoc-round key table)
  (if (null? table)
      false
      (let ((record (car table)))
        (if (= (round key) (round (car record)))
            record
            (assoc-round key (cdr table))))))
(define t2 (make-table->same-key assoc-round))
((t2 'lookup-proc) 1.2)
((t2 'insert-proc!) 1.2 "first")
((t2 'lookup-proc) 1.3)
((t2 'insert-proc!) 2.4 "second")
((t2 'lookup-proc) 1.9)


;; --- 3.25 Final iteration --- ;;
(define (make-table->general)
  (let ((local-table (list '*table*)))
    (define (lookup keys)
      (define (internal-lookup keys table)
        (let ((key (car keys)))
          (let ((record (assoc key (cdr table))))
            (if record
                (if (not (null? (cdr keys)))
                    (internal-lookup (cdr keys) record)
                    (cdr record))
                false))))
      (internal-lookup keys local-table))
    (define (insert! keys val)
      (define (internal-insert keys val table)
       (let ((key (car keys)))
         (if (null? (cdr keys))
             ;; last key, create a record
             (let ((record (assoc key (cdr table))))
               (if record
                   (set-cdr! record val)
                   (set-cdr! table (cons (cons key val)
                                         (cdr table)))))
             ;; more keys left, looking at subtables
             (let ((record (assoc key (cdr table))))
               (if record
                   ;; i already have a subtable or record here, cdr could be a pair or val, if its a val it needs to become a pair
                   (if (pair? (cdr record))
                       ;; record is a list already
                       (internal-insert (cdr keys) val record)
                       ;; else i have a k-v and need to create a new subtable
                       (let ((subtable (list key)))
                         (internal-insert (cdr keys) val subtable)
                         (set-cdr! record (cdr subtable))))
                   ;; no record here, new subtable
                   (let ((subtable (list key)))
                     (internal-insert (cdr keys) val subtable)
                     (set-cdr! table (cons subtable
                                           (cdr table)))))))))
      (internal-insert keys val local-table))
    (define (show)
      (display local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'show) show)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define t4 (make-table->general))
((t4 'lookup-proc) (list 'one))
((t4 'insert-proc!) (list 'one) 1)
((t4 'insert-proc!) (list 'one 'two) 2)
((t4 'lookup-proc) (list 'one 'two))
((t4 'insert-proc!) (list 'one 'three) 3)
((t4 'lookup-proc) (list 'one 'three))
((t4 'insert-proc!)  (list 'one 'two 'four) 4)
((t4 'lookup-proc) (list 'one 'two 'four))
((t4 'show))
((t4 'insert-proc!) (list 'one 'three 'five 'six) 6)
((t4 'lookup-proc) (list 'one 'three 'five 'six))



;; Simulator for Digital Circuits
(define a (make-wire))
(define b (make-wire))
(define c (make-wire))
(define d (make-wire))
(define e (make-wire))
(define s (make-wire))

(or-gate a b d)
(and-gate a b c)
(inverter c e)
(and-gate d e s)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  ;; c-in is the incomine carry bit
  ;; c-out is the outgoing carry bit
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))


(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (logical-or s1 s2)
  (or (= s1 1) (= s2 1)))


(define (or-gate->and-gates-and-inverters a1 a2 output)
  (define (or-action-procedure)
    (let ((c (make-wire))
          (d (make-wire))
          (e (make-wire)))
      (inverter a1 c)
      (inverter a2 d)
      (and-gate c d e)
      (inverter e output)))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure))


(define (ripple-carry-adder a-wires b-wires s-wires cin)
  (let (cout (make-wire))
    (if (null? a-wires)
        'ok
        (let ((fa (full-adder (car a-wires)
                              (car b-wires)
                              cin
                              (car s-wires)
                              cout)))
          (ripple-carry-adder (cdr a-wires)
                              (cdr b-wires)
                              (cdr s-wires)
                              cout)))))

;; Representing wires
(define (make-wire)
  (let ((signal-value 0)
        (action-procedures '()))
    (define (set-my-signal! new-value)
      ;; test if signal changes because if it does not we do not want to re-run all the procedures on the wire
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

((define (call-each procedures)
   (if (null? procedures)
       'done
       (begin
         ((car procedures))
         (call-each (cdr procedures))))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

(define the-agenda (make-agenda))
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)

(define input-1 (make-wire))
(define input-2 (make-wire))
(define sum (make-wire))
(define carry (make-wire))
(probe 'sum sum)
(probe 'carry carry)

(half-adder input-1 input-2 sum carry)

(set-signal! input-1 1)

(propagate)

;; Implementing the agenda
;;  - the agenda is made up of time segments, each time segment is a pair consisting of a number (the time) and a queue that holds procedures scheduled to be run during that time segment
(define (make-time-segment time queue)
  (cons time queue))
(define (segment-time s) (car s))
(define (segment-queue s) (cdr s))


;; Store the current time (time of last action processed) at the head of the list
;; the agenda is a one-dimensional table of time segments, a headed list, headed by the current time
(define (make-agenda) (list 0))
(define (current-time agenda) (car agenda))
(define (set-current-time! agenda time)
  (set-car! agenda time))
(define (segments agenda) (cdr agenda))
(define (set-segments! agenda segments)
  (set-cdr! agenda segments))
(define (first-segment agenda) (car (segments agenda)))
(define (rest-segments agenda) (cdr (segments agenda)))
(define (empty-agenda? agenda)
  (null? (segments agenda)))


(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
        ;; insert the action into the procedure queue of the segment
        (insert-queue! (segment-queue (car segments))
                       action)
        (let ((rest (cdr segments)))
          (if (belongs-before? rest)
              (set-cdr!
               segments
               (cons (make-new-time-segment time action)
                     (cdr segments)))
              (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (cons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (error "Agenda is empty -- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))

;; 3.3.5 Propagation of Constraints
(define C (make-connector))
(define F (make-connector))
(celsius-fahrenheit-converter C F)

(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))

;; placing a 'probe' on a connector will cause a message to be printed whenever the connector is given a value
(probe "Celsius temp" C)
(probe "Fahrenheit temp" F)

(set-value! C 25 'user)

;; An adder constructs an adder constraint with two addend connectors and a sum connector
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)
;; procedure 'me' represents the adder and acts as a dispatch to the local procedures

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0 me))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? product) (has-value? m1))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? product) (has-value? m2))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (newline)
    (display "Probe: ")
    (display name)
    (display " = ")
    (display value))
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)

(define (make-connector)
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (display "SETTING") (newline)
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (error "Contradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignored))
    (define (connect new-constraint)
      (if (not (memq new-constraint constraints))
          (set! constraints
                (cons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    (define (me request)
      (cond ((eq? request 'has-value?)
             (if informant
                 true
                 false))
            ((eq? request 'value) value)
            ((eq? request 'set-value!) set-my-value)
            ((eq? request 'forget) forget-my-value)
            ((eq? request 'connect) connect)
            (else (error "Unknown operation -- CONNECTOR" request))))
    me))

;; apply a procedure to all items in a list except a given one
(define (for-each-except exception procedure list)
  (define (loop items)
    (display "List: ") (display list) (newline)
    (cond ((null? items) 'done)
          ((eq? (car items) exception)
           (loop (cdr items)))
          (else (procedure (car items))
                (loop (cdr items)))))
  (loop list))

;; the following procedures provide a syntax interface for the connector dispatch
(define (has-value? connector)
  (connector 'has-value?))
(define (get-value connector)
  (connector 'value))
(define (set-value! connector new-value informant)
  ((connector 'set-value!) new-value informant))
(define (forget-value! connector retractor)
  ((connector 'forget) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))

(define (averager a b c)
  (let ((sum (make-connector))
        (num-args (make-connector)))
    (constant 2 num-args)
    (adder a b sum)
    (multiplier num-args c sum)))

(define arg1 (make-connector))
(define arg2 (make-connector))
(define result (make-connector))
(constant 3 arg1)
(constant 5 arg2)
(averager arg1 arg2 result)
(has-value? result)
(get-value result)

(define (squarer a b)
  (define (process-new-value)
    (if (has-value? b)
        (if (< (get-value b) 0)
            (error "square less than 0 -- SQUARER" (get-value b))
            (set-value! a (sqrt (get-value b)) me))
        (if (has-value? a)
            (let ((a-val (get-value a)))
              (set-value! b
                          (* a-val a-val)
                          me)))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (error "Unknown request -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)

(define arg (make-connector))
(define result (make-connector))
(constant 3 arg)
(squarer arg result)
(has-value? result)
(get-value result)


(define (celsius-fahrenheit-converter x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define C (make-connector))
(define F (celsius-fahrenheit-converter C))

(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c- x y)
  (let ((z (make-connector)))
    (adder z y x)
    z))
(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))
(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier z y x)
    z))
(define (cv x)
  (let ((z (make-connector)))
    (constant x z)
    z))

;; serializers
(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell)
  (set-car! cell false))

;; this 'test-and-set!' is not sufficient as we need to guarantee it is performed atomically
(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;; An atomic implementation in MIT Scheme
(define (test-and-set! cell)
  (without-interrupts
   (lambda ()
     (if (car cell)
         true
         (begin (set-car! cell true)
                false)))))

(define (make-semaphore n)
  (let ((count n)
        (mutex (make-mutex)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (mutex 'acquire)
             (if (> count 0)
                 (begin
                   (set! count (- count 1))
                   (mutex 'release))
                 (begin
                   (mutex 'release)
                   ; this is a busy wait and in a single threaded environment will infinitely loop
                   (the-semaphore 'acquire))))
            ((eq? m 'release)
             (mutex 'acquire)
             (set! count (- count 1))
             (mutex 'release))))
    the-semaphore))

(define my-sem (make-semaphore 3))
(begin
  (my-sem 'acquire)
  (display "one") (newline)
  (my-sem 'acquire)
  (display "two") (newline)
  (my-sem 'acquire)
  (display "three") (newline)
  (my-sem 'release)
  (my-sem 'release)
  (my-sem 'release))

(define (make-semaphore n)
  (let ((count n)
        (cell (list false)))
    (define (the-semaphore m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell) ; acquire the lock
                 (the-semaphore 'acquire)
                 (if (> count 0)
                     (begin
                       (set! count (- count 1))
                       (clear! cell))
                     (the-semaphore 'acquire))))
            ((eq? m 'release)
             (if (test-and-set! cell)
                 (the-semaphore 'release)
                 (begin
                   (set! count (+ count 1))
                   (clear! cell))))))
    the-semaphore))


(define the-sem (make-semaphore 3))
(begin
  (the-sem 'acquire)
  (display "one") (newline)
  (the-sem 'acquire)
  (display "two") (newline)
  (the-sem 'acquire)
  (display "three") (newline)
  (the-sem 'acquire)
  (display "four") (newline)
  (the-sem 'acquire)
  (display "five") (newline)
  (the-sem 'release)
  (the-sem 'release))


;; Streams
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map  proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (display-stream s)
  (stream-for-each display-line s))

(define the-empty-stream '())
; cons-stream must be a special form to avoid the evaluation of the second argument
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

;; finding second prime in interval
(stream-car
 (stream-cdr
  (stream-filter prime?
                 (stream-enumerate-interval 10000 1000000))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))

(define (display-line x)
  (display x) (newline))

(define (show x)
  (display-line x)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7)
(display-stream z)
sum

(define (integers-starting-from n)
  (cons-stream n (integers-starting-from (+ n 1))))

(define (divisible? x y) (= (remainder x y) 0))

(define (sieve stream)
  (cons-stream
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))

(define primes (sieve (integers-starting-from 2)))
(stream-ref primes 50)

(define ones (cons-stream 1 ones))
(define (add-streams s1 s2)
  (stream-map + s1 s2))
(define integers (cons-stream 1 (add-streams ones integers)))

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))

(define double (cons-stream 1 (scale-stream double 2)))

(define primes
  (cons-stream
   2
   (stream-filter prime? (integers-starting-from 3))))
;; recursive definition because 'primes is defined in terms of 'prime?' which itself relies on the `primes` stream
(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes))


(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define integers (cons-stream 1 (add-streams ones integers)))
(define factorials (cons-stream 1 (mul-streams factorials integers)))

(stream-ref factorials 3) ; 6
(stream-ref factorials 5) ; 120


(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s)
                            (partial-sums s))))

(define partial-ints (partial-sums integers))
(stream-ref partial-ints 4)


(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define S (cons-stream 1 (merge (scale-stream S 2)
                                (merge (scale-stream S 3)
                                       (scale-stream S 5)))))


(define (expand num den radix)
  (cons-stream
   (quotient (* num radix) den)
   (expand (remainder (* num radix) den) den radix)))

(expand 1 7 10)
(expand 3 8 10)
(stream-ref (expand 3 8 10) 0)
(stream-ref (expand 2 1 2) )

;; from chap 1
(define (average x y)
  (/ (+ x y) 2))
(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


(define (make-tableau transform s)
  (cons-stream s
               (make-tableau transform
                             (transform s))))
(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))

(define (stream-limit s tol)
  (let ((s1 (stream-ref s 0))
        (s2 (stream-ref s 1)))
    (if (< (abs (- s1 s2)) tol)
        s2
        (stream-limit (stream-cdr s) tol))))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))
(sqrt 2 0.001)
(stream-ref (sqrt-stream 2) 2)

(stream-filter (lambda (pair)
                 (prime? (+ (car pair) (cadr pair))))

               int-pairs)



(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons-stream (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))

(define (pairs s t)
  (cons-stream
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (pairs (stream-cdr s) (stream-cdr t)))))

(define (display-line x)
  (display x) (newline))
(define (show-stream s limit)
  (define (display-iter st count)
    (if (> count limit)
        'done
        (begin
          (display-line (stream-car st))
          (display-iter (stream-cdr st) (+ count 1)))))
  (display-iter s 0))

(define integers
  (cons-stream 1 (stream-map (lambda (x) (+ x 1))
                             integers)))

(stream-ref (pairs integers integers) 5)
(show-stream (pairs integers integers) 10)



(define (triples a b c)
  (cons-stream
   (list (stream-car a) (stream-car b) (stream-car c))
   (interleave
    (stream-map (lambda (x)
                  (append (list (stream-car a)) x))
                (pairs b c))
    (triples (stream-cdr a) (stream-cdr b) (stream-cdr c)))))


(define triple-stream (triples integers integers integers))
(show-stream triple-stream 10)

(define pythagorean-triples
  (stream-filter (lambda (t)
                   (= (+ (square (car t)) (square (cadr t)))
                      (square (caddr t))))
                 trips))

(show-stream pythagorean-triples 5)

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons-stream s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (cons-stream s2car (merge s1 (stream-cdr s2))))
                 (else
                  (cons-stream s1car
                               (merge (stream-cdr s1)
                                      (stream-cdr s2)))))))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (let ((w1 (weight s1car))
                 (w2 (weight s2car)))
             ;; if we instead use an 'else' case as we do in 'merge' then we skip over valid values in the s2 stream
             ;; since equal weights can be created from different pairs, e.g. weight 5 from (1 4) and (2 3)
             (cond ((<= w1 w2)
                    (cons-stream s1car
                                 (merge-weighted
                                  (stream-cdr s1)
                                  s2
                                  weight)))
                   ((> w1 w2)
                    (cons-stream s2car
                                 (merge-weighted
                                  s1
                                  (stream-cdr s2)
                                  weight)))))))))

(define (weighted-pairs s1 s2 weight)
  (cons-stream
   (list (stream-car s1) (stream-car s2))
   (merge-weighted
    (stream-map (lambda (x)
                  (list (stream-car s1) x))
                (stream-cdr s2))
    (weighted-pairs (stream-cdr s1) (stream-cdr s2) weight)
    weight)))

(define a (weighted-pairs integers integers (lambda (x) (apply + x))))

(define (div-2-3-5 n)
  (or (= 0 (remainder n 2))
      (= 0 (remainder n 3))
      (= 0 (remainder n 5))))

(define b (stream-filter
           (lambda (p)
             (let ((i (car p))
                   (j (cadr p)))
               (and (not (div-2-3-5 i))
                    (not (div-2-3-5 j)))))
           (weighted-pairs integers integers
                           (lambda (p)
                             (let ((i (car p))
                                   (j (cadr p)))
                               (+ (* i 2) (* j 3) (* i j 5)))))))

(show-stream a 10)
(show-stream b 10)

(define (cube n)
  (* n n n))
(define (stream-search s weight)
  (if (= (weight (stream-car s))
         (weight (stream-car (stream-cdr s))))
      (cons-stream
       (weight (stream-car s))
       (stream-search (stream-cdr s) weight))
      (stream-search (stream-cdr s) weight)))
(define (cube-weight p)
  (let ((i (car p))
        (j (cadr p)))
    (+ (cube i)
       (cube j))))

(define ramanujan-numbers
  (stream-search
   (weighted-pairs integers integers cube-weight)
   cube-weight))

(show-stream ramanujan-numbers 6)


(define (integral integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (add-streams (scale-stream integrand dt)
                              int)))
  int)

(define (RC R C dt)
  (define (proc i v0)
    (cons-stream
     v0
     (add-streams (scale-stream i R)
                  (integral (scale-stream i (/ 1 C))
                            v0
                            dt))))
  proc)
(define RC1 (RC 5 1 0.5))
(show-stream (RC1 integers 0.2) 10)


(define (make-zero-crossings input-stream last-value)
  (cons-stream
   (sign-change-detector (stream-car input-stream) last-value)
   (make-zero-crossings (stream-cdr input-stream)
                        (stream-car input-stream))))
(define zero-crossings (make-zero-crossings sense-data 0))

(define zero-crossings
  (stream-map sign-change-detector sense-data (cons-stream 0 sense-data)))


(define (make-zero-crossings input-stream last-value last-average)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons-stream (sign-change-detector avpt last-average)
                 (make-zero-crossings (stream-cdr input-stream)
                                      (stream-car input-stream)
                                      avpt))))


(define (smooth input-stream last-value)
  (define (average x y)
    (/ (+ x y) 2))
  (let ((average-val (average (stream-car input-stream) last-value)))
    (cons-stream average-val
                 (smooth (stream-cdr input-stream) (stream-car input-stream)))))

(show-stream (smooth integers 0) 10)

(define (make-zero-crossings source-signal smooth)
  (define (check-zero-crossings input-stream last-value)
    (cons-stream (sign-change-detector (stream-car input-stream) last-value)
                 (check-zero-crossings (stream-cdr input-stream) (stream-car input-stream))))
  (check-zero-crossings (smooth source-signal) 0))

;; 3.5.4
(define (integral delayed-integrand initial-value dt)
  (define int
    (cons-stream initial-value
                 (let ((integrand (force delayed-integrand)))
                   (add-streams (scale-stream integrand dt)
                                int))))
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

(define (integral delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                    the-empty-stream
                    (integral (delay (stream-cdr integrand))
                              (+ (* dt (stream-car integrand))
                                 initial-value)
                              dt)))))

(stream-ref (solve (lambda (y) y) 1 0.001) 1000)

(define (solve-2nd a b dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (add-streams (scale-stream dy a)
                           (scale-stream y b)))
  y)

(stream-ref (solve-2nd 1 0 0.001 1 1) 1000)

(define (solve-2nd f dt y0 dy0)
  (define y (integral (delay dy) y0 dt))
  (define dy (integral (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

(define (RLC R L C dt)
  (define (proc vC0 iL0)
    (define vC (integral (delay dvC) vC0 dt))
    (define iL (integral (delay diL) iL0 dt))
    (define dvC (scale-stream iL (/ -1 C)))
    (define diL (add-streams (scale-stream iL (/ (* R -1) L))
                            (scale-stream vC (/ 1 L))))
    (cons vC iL))
  proc)

(define test-RLC (RLC 1 1 0.2 0.1))
(define str-pairs (test-RLC 10 0))
(stream-ref (car str-pairs) 5)
(stream-ref (cdr str-pairs) 5)
