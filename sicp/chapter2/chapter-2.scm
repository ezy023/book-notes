(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (numer y) (denom x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))


(define x (cons 1 2))
(define y (cons 3 4))

(car x)
(cdr x)

(define z (cons x y))

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))
(define (print-rat x)
  (display (numer x))
  (display "/")
  (display (denom x)))
(define (make-rat n d)
  (let* ((g (gcd n d))
         (ng (/ n g))
         (dg (/ d g)))
    (cond ((and (< n 0) (< d 0))
           (cons ng dg))
          ((or (< n 0) (< d 0))
           (cons (- ng) (abs dg)))
          (else (cons ng dg)))))

(print-rat (make-rat 1 2))
(print-rat (make-rat -1 2))
(print-rat (make-rat -1 -2))
(print-rat (make-rat 1 -2))

(define (make-segment start end)
  (cons start end))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (make-point x y) (cons x y))
(define (x-point point) (car point))
(define (y-point point) (cdr point))

(define (average x y) (/ (+ x y) 2))
(define (midpoint-segment segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define start (make-point 0 0))
(define end (make-point 10 10))
(define seg (make-segment start end))
(define mid (midpoint-segment seg))
(print-point mid)


(define (make-rect start end)
  (make-segment start end))
(define (height-rect segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (abs (- (x-point start) (x-point end)))))
(define (width-rect segment)
  (let ((start (start-segment segment))
        (end (end-segment segment)))
    (abs (- (y-point start) (y-point end)))))
(define (area-rect rect)
  (let ((h (height-rect rect))
        (w (width-rect rect)))
    (* h w)))
(define (perim-rect rect)
  (let ((h (height-rect rect))
        (w (width-rect rect)))
  (+ (* h 2) (* w 2))))

(define rect (make-rect
              (make-point 2 2)
              (make-point 6 7)))

(area-rect rect)
(perim-rect rect)

(define (make-rect-2 height width)
  (cons height width))
(define (height-rect rect) (car rect))
(define (width-rect rect) (cdr rect))

(define rect2 (make-rect-2 4 6))
(area-rect rect2)
(perim-rect rect2)


(define (econs x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

((lambda (m) (m x y))
 (lambda (p q) p))
((lambda (m)
   ((lambda (p q) p) x y)))

((lambda (m)
   ((lambda (p q) x) x y)))

(define (cdr z)
  (z (lambda (p q) q)))

(define (pcons a b)
  (* (expt 2 a) (expt 3 b)))

(define (pcar z)
  (/ (2 a) (3 b)))


(define (make-interval a b) (cons a b))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))


(define (lower-bound intv) (car intv))
(define (upper-bound intv) (cdr intv))


(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))


(define (width interval)
  (- (upper-bound interval) (lower-bound interval)))
(define (sum-interval-widths x y))

(add-interval (make-interval 1 5)
              (make-interval 3 7))
(add-interval (make-interval 2 5)
              (make-interval 3 6))

(sub-interval (make-interval 1 5)
              (make-interval 3 7))
(sub-interval (make-interval 2 5)
              (make-interval 3 6))

(define (check-widths a b)
  (if (= (width a) (width b))
      #t
      #f))


(define interval-one (make-interval 1 5))
(define interval-two (make-interval 3 7))
(if (= (width (add-interval interval-one interval-two))
       (+ (width interval-one) (width interval-two)))
    #t
    #f)

(if (= (width (sub-interval interval-one interval-two))
       (+ (width interval-one) (width interval-two)))
    #t
    #f)

(if (= (width (mul-interval interval-one interval-two))
       (+ (width interval-one) (width interval-two)))
    #t
    #f)

(if (= (width (div-interval interval-one interval-two))
       (+ (width interval-one) (width interval-two)))
    #t
    #f)

(define (width interval)
  (- (upper-bound interval) (lower-bound interval)))

(define (span-zero? x)
  (> (width x) (upper-bound x)))

(define (div-interval x y)
  (if (span-zero? y)
      (error "Attempting to divide with an interval that spans 0")
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))

(div-interval (make-interval 1.0 4.0)
              (make-interval 2.0 5.0))
(div-interval (make-interval -1.0 4.0)
              (make-interval 2.0 5.0))
(div-interval (make-interval 1.0 4.0)
              (make-interval -2.0 5.0))


(define (make-center-percent center percent)
  (let ((diff (* center (/ percent 100.0))))
    (make-interval (- center diff) (+ center diff))))
(define (percent i)
  (let ((half-width (/ (- (upper-bound i) (lower-bound i)) 2)))
    (* (/ half-width (+ (lower-bound i) half-width))
       100)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))

(define i (make-center-percent 10 20))
(percent i)
(center i)


(define (reverse l)
  (if (null? l)
      l
      (cons (reverse (cdr l)) (car l))))

(reverse (list 1 2 3 4 5))



(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define first-denomination car)
(define except-first-denomination cdr)
(define no-more? null?)

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

(define (same-parity i . j)
  (define (iter-list l)
    (cond ((null? l) l)
          ((= (remainder i 2) (remainder (car l) 2))
           (cons (car l) (iter-list (cdr l))))
          (else (iter-list (cdr l)))))
  (iter-list j))

(same-parity 1 2 3 4 5 6 7 8 9 10)
(same-parity 2 3 4 5 6 7 8 9 10 11)

(define (square x) (* x x))
(define nil ())
(define (square-list-one items)
  (if (null? items)
      nil
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list-two items)
  (map square items))

(square-list-one (list 1 2 3 4))
(square-list-two (list 1 2 3 4))

(define (my-for-each proc items)
  (cond ((null? items) #t)
        (else
         (proc (car items))
         (my-for-each proc (cdr items)))))

(my-for-each (lambda (x) (newline) (display x))
             (list 57 321 88))

(cons (list 1 2) (list 3 4))

(pair? (list 1 2))
(pair? (cons 1 2))
(pair? (car (list 1 2)))
(pair? (car (cons 1 2)))
(pair? (cdr (list 1 2)))
(pair? (cdr (cons 1 2)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(list 1 (list 2 (list 3 4)))

(car (cdr (car (cdr (cdr '(1 3 (5 7) 9))))))
(car (car '((7))))
(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr
                                                    '(1 (2 (3 (4 (5 (6 7))))))
                                                    ))))))))))))


(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)
; (1 2 3 4 5 6)
(cons x y)
; ((1 2 3) 4 5 6)
(list x y)
; ((1 2 3) (4 5 6))

(list '(1 2) 3)
(pair? (cdr (list 1 2 3 4)))
(define (reverse l)
  (if (null? l)
      l
      (append (reverse (cdr l)) (list (car l)))))
(reverse '(1 2 3 4 5))




(define (deep-reverse items)
  (cond ((null? items) items)
        ((pair? (car items))
         (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))
        (else
         (append (deep-reverse (cdr items)) (list (car items))))))

(define x (list (list 1 2) (list 3 4)))

(deep-reverse x)
(deep-reverse '( 1 2 3 4 5))
(pair? 1)

(define (fringe items)
  (cond ((null? items) items)
        ((pair? items)
         (append (fringe (car items)) (fringe (cdr items))))
        (else (list items))))

(define x (list (list 1 2) (list 3 4)))
(fringe x)
(fringe (list x x))


(define (make-mobile left right)
  (list left right))
; structure is either a number (for a weight) or another mobile
(define (make-branch length structure)
  (list length structure))


(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))



(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
(define (total-weight structure)
  (cond ((not (pair? structure))
         structure)
        (else (+ (total-weight (branch-structure (left-branch structure)))
                 (total-weight (branch-structure (right-branch structure)))))))

(define m (make-mobile (make-branch 1 2)
                       (make-branch 3 4)))
(define large-m (make-mobile (make-branch 1 m)
                             (make-branch 2 m)))
(define larger-m (make-mobile (make-branch 1 m)
                              (make-branch 2 large-m)))
(total-weight m)
(total-weight large-m)
(total-weight larger-m)


(cdr (list 1 2))
(cdr (cons 1 2))

(define (branch-torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))
(define (balanced? structure)
  (cond ((not (pair? structure))
         #t)
        ((= (branch-torque (left-branch structure)) (branch-torque (right-branch structure)))
         (and (balanced? (branch-structure (left-branch structure)))
              (balanced? (branch-structure (right-branch structure)))))
        (else
         #f)))

(define b (make-mobile (make-branch 3 4)
                        (make-branch 2 6)))
(define db (make-mobile (make-branch 3 b)
                        (make-branch 2 b)))
(balanced? b)
(balanced? db)

(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree (car tree))
                    (square-tree (cdr tree))))))

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))

(define (tree-map tree transform)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map sub-tree transform)
             (transform sub-tree)))
       tree))

(define (square-tree tree) (tree-map tree square))

(define t (list 1
                (list 2 (list 3 4) 5)
                (list 6 7)))
(square-tree t)


(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (acc-map p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) nil sequence))

(acc-map (lambda (x) (+ 2 x)) '(1 2 3 4 5 6 7 8 9))

(define (acc-append seq1 seq2)
  (accumulate cons seq2 seq1))

(acc-append '(1 2 3) '(4 5 6))

(define (acc-length sequence)
  (accumulate (lambda (x y) (+ y 1)) 0 sequence))

(acc-length '(1 2 3 4 5 6 7 8 9 10))

(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ (* x higher-terms) this-coeff))
              0
              coefficient-sequence))

(horner-eval 2 (list 1 3 0 5 0 1))


(define my-tree (list 1 (list 2
                              (list 3 4))
                      (list 5 6)))
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree)) 1)
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(count-leaves my-tree)

(define (acc-count-leaves t)
  (accumulate + 0 (map (lambda (x)
                         (if (pair? x)
                             (acc-count-leaves x)
                             1))
                       t)))

(acc-count-leaves my-tree)

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(define s (list '(1 2 3) '(4 5 6) '(7 8 9) '(10 11 12)))
(accumulate-n + 0 s)


(define matrix (list '(1 2 3 4) '(4 5 6 6) '(6 7 8 9)))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (row) (dot-product v row)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (row) (matrix-*-vector cols row)) m)))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)

(fold-right / 1 (list 1 2 3)) ; 3/2
(fold-left / 1 (list 1 2 3))  ; 4/3
(fold-right list '() (list 1 2 3))
(fold-left list '() (list 1 2 3))


(define (reverse-right sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              '() sequence))

(define (reverse-left sequence)
  (fold-left (lambda (x y)
               (append (list y) x))
             '() sequence))

(define s (list 1 2 3 4 5))
(reverse-left s)
(reverse-right s)



(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(enumerate-interval 1 6)

(accumulate append
            '()
            (map (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
                 (enumerate-interval 1 6)))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

;; prime? procedures from chapter 1
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? n)
  (fast-prime? n 5))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

(prime-sum? '(2 1))

(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (flatmap
                (lambda (i)
                  (map (lambda (j) (list i j))
                       (enumerate-interval 1 (- i 1))))
                (enumerate-interval 1 n)))))
(prime-sum-pairs 6)

(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (my-remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (my-remove x s))))
               s)))

(permutations '(1 2 3))


(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (unique-triples n)
  (flatmap (lambda (i)
             (map (lambda (x) (cons i x))
                  (flatmap (lambda (j)
                             (map (lambda (k)
                                    (list j k))
                                  (enumerate-interval 1 (- j 1))))
                           (enumerate-interval 1 (- i 1)))))
           (enumerate-interval 1 n)))


(unique-triples 6)


(define (make-triple-sum t)
  (+ (car t) (cadr t) (caddr t)))

(define (triples-sum s n)
  (filter (lambda (t)
            (= (make-triple-sum t) s))
          (unique-triples n)))

(triples-sum 8 6)
(triples-sum 7 6)
(triples-sum 11 6)
(triples-sum 10 6)


(define (adjoin-position row col positions)
  (cons (make-position row col) positions))
(define empty-board '())

(define (make-position r c)
  (cons r c))
(define (row p)
  (car p))
(define (col p)
  (cdr p))

(define (on-diagonal? cp pos)
  (= (abs (- (row cp) (row pos)))
     (abs (- (col cp) (col pos)))))

(define (safe2? c positions)
  (let ((kth-queen (car positions))
        (rest-queens (filter (lambda (pos)
                               (not (= c (col pos))))
                             positions)))
    (define (iter q board)
      (or (null? board)
          (and (not (= (row kth-queen) (row (car board))))
               (not (on-diagonal? kth-queen (car board)))
               (iter q (cdr board)))))
    (iter kth-queen rest-queens)))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe2? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(length (queens 8))

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x2) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))
(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))


(define (my-equal? a b)
  (cond ((and (null? a) (null? b))
         #t)
        ((or (null? a) (null? b))
         #f)
        ((and (pair? a) (pair? b))
         (and (eq? (car a) (car b))
              (my-equal? (cdr a) (cdr b))))
        (else (eq? a b))))

(my-equal? '(this is a list) '(this is a list))
(my-equal? '(this is a list) '(this (is a) list))
(my-equal? 1 1)
(my-equal? 2 1)
(my-equal? '(a b) '(a c))
(my-equal? '(a b) '(a b))

(car ''abracadabra)
''abracadabra

''(a b)
'(a b)
(quote (a b))
(equal? '(a b) (quote (a b)))

(environment-bound? (nearest-repl/environment) 'eval)
;; Symbolic differentiation

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIVE" exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

;; simplify expressions
(define (=number? exp num)
  (and (number? exp) (= exp num)))
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(deriv '(+ x 3) 'x)
(deriv '(* x y) 'x)
(deriv '(* (* x y) (+ x 3)) 'x)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (make-exponentiation (make-product (exponent exp) (base exp)) (- (exponent exp) 1))
                       (deriv (base exp) var)))
        (else
         (error "unknown expression type -- DERIVE" exp))))
(define (exponentiation? exp)
  (and (pair? exp) (eq? (car exp) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))
(define (make-exponentiation base expt)
  (cond ((=number? expt 0) 1)
        ((=number? expt 1) base)
        (else (list '** base expt))))

(deriv '(** x 2) 'x)
(deriv '(** x 3) 'x)


(define erik '(* x y (+ x 3)))

(list '+ (cddr erik))
(append (list '+) (cddr erik))



(define (augend x)
  (if (null? (cdddr x))
      (caddr x)
      (cons '+ (cddr x))))
(define (multiplicand x)
  (if (null? (cdddr x))
      (caddr x)
      (cons '* (cddr x))))
(deriv '(* x y (+ x 3)) 'x)


(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))



(define (addend s) (car s))
(define (augend s) (caddr s))
(define (multiplier p) (car p))
(define (multiplicand p) (caddr p))
(define (sum? x)
  (and (pair? x) (eq? (cadr x) '+)))
(define (product? x)
  (and (pair? x) (eq? (cadr x) '*)))

(deriv '(x * y) 'x)
(deriv '(x + (3 * (x + (y + 2)))) 'x)

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((not (element-of-set? (car set1) set2))
         (cons (car set1)
               (union-set (cdr set1) set2)))
        (else (union-set (cdr set1) set2))))


(element-of-set? 2 '(1 2 3))
(adjoin-set 4 '(1 2 3))
(intersection-set '(1 2 3 4) '(3 4 5 6))
(union-set '(1 2 3 4) '(3 4 5 6))


(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set) (cons x set))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

(define (union-set set1 set2)
  (append set1 set2))

;; Sets as ordered lists
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set (cdr set1)
                                       (cdr set2))))
              ((< x1 x2)
               (intersection-set (cdr set1) set2))
              ((> x1 x2)
               (intersection-set set1 (cdr set2)))))))

(define (adjoin-set x set)
  (cond ((element-of-set? x set)
         set)
        ((null? set) (cons x '()))
        ((< x (car set))
         (cons x set))
        ((> x (car set))
         (cons (car set) (adjoin-set x (cdr set))))))

(adjoin-set 4 '(1 3 5 7))
(adjoin-set 1 '(3 5 7))
(adjoin-set 7 '(1 3 5))
(adjoin-set 3 '(1 3 5 7))
(adjoin-set 2 '())

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1)) (x2 (car set2)))
           (cond ((= x1 x2) (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2) (cons x1 (union-set (cdr set1) set2)))
                 (else (cons x2 (union-set set1 (cdr set2)))))))))

(union-set '(1 3 5 7) '(2 4 6 8))
(union-set '(1 2 3) '(4))
(union-set '(1 2) '(3 4))
(union-set '(1) '())

;; Sets as binary trees
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) falst)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))



; Unfinished ex 2.65
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((= (car set1) (car set2))
         (make-tree (car set1)
                    (intersection-set (left-branch set1)
                                      (left-branch set2))
                    (intersection-set (right-branch set1)
                                      (right-branch set2))))
        ((< (car set1) (car set2))
         (intersection-set (left-branch set1)
                           (left-branch set2)))
        ((> (car set1) (car set2))
         (intersection-set (right-branch set1)
                           (right-branch set2)))))


(define t1 (make-tree 5
                      (make-tree 1 '() (make-tree 3 '() '()))
                      (make-tree 9
                                 (make-tree 7 (make-tree 6 '() '()) '())
                                 (make-tree 10 '() '()))))

(define t2 (make-tree 5
                      (make-tree 1 '() (make-tree 3 '() '()))
                      (make-tree 11 (make-tree 7
                                               (make-tree 6 '() '())
                                               (make-tree 10 '() '())) '())))

(intersection-set t1 t2)


(define (lookup given-key set-of-records)
  (cond ((null? set-of-records) false)
        (else
         (let ((cur-key (key (entry set-of-records))))
           (cond ((= given-key cur-key)
                  (entry set-of-records))
                 ((< given-key cur-key)
                  (lookup given-key (left-branch set-of-records)))
                 ((> given-key cur-key)
                  (lookup given-key (right-branch set-of-records))))))))


;; Huffman encoding trees
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))
(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))


(define sample-tree
  (make-code-tree (make-leaf 'A 6)
                  (make-code-tree (make-code-tree (make-leaf 'B 3)
                                                  (make-code-tree (make-leaf 'C 1)
                                                                  (make-leaf 'D 1)))
                                  (make-code-tree (make-code-tree (make-leaf 'E 1)
                                                                  (make-leaf 'F 1))
                                                  (make-leaf 'null 0)))))

(define deadbeef '(1 0 1 1 1 1 0 0 0 1 0 1 1 1 0 0 1 1 0 0 1 1 0 0 1 1 0 1))
(define facade '(1 1 0 1 0 1 0 1 0 0 1 0 1 1 1 1 0 0))
(define decade '(1 0 1 1 1 1 0 0 1 0 1 0 0 1 0 1 1 1 1 0 0))
(decode deadbeef sample-tree)
(decode facade sample-tree)
(decode decade sample-tree)


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (element-in-set? x set)
  (cond ((null? set) false)
        ((eq? x (car set)) true)
        (else (element-in-set? x (cdr set)))))

(symbols sample-tree)
(element-in-tree? 'A (symbols sample-tree))
(element-in-tree? 'H (symbols sample-tree))

(define (encode-symbol symbol tree)
  (cond ((leaf? tree)
         '())
        ((element-in-set? symbol (symbols (left-branch tree)))
         (append (list 0)
                 (encode-symbol symbol (left-branch tree))))
        ((element-in-set? symbol (symbols (right-branch tree)))
         (append (list 1)
                 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree -- ENCODE-SYMBOL " symbol))))


(define sample-pairs '((A 5) (B 3) (C 1) (D 1) (E 1) (F 1)))

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
  (cond ((null? leaf-set) '())
        ((null? (cdr leaf-set)) (car leaf-set))
        (else
         (successive-merge
          (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set))
                      (cddr leaf-set))))))

(define huff-tree (generate-huffman-tree sample-pairs))
(decode (encode '(F A C A D E) huff-tree) huff-tree)

(define song-pairs '((a 2) (boom 1) (get 2) (job 2) (na 16) (sha 3) (yip 9) (wah 1)))
(define song-tree (generate-huffman-tree song-pairs))
(define song-message '(get a job
                           sha na na na na na na na na
                           get a job
                           sha na na na na na na na na
                           wah yip yip yip yip yip yip yip yip yip
                           sha boom))
(length (encode song-message song-tree))
(encode song-message song-tree)
(* (length song-message) 3)


; Complex numbers

(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                        (+ (imag-part z1) (imag-part z2))))
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


(define (attach-tag type-tag contents)
  (cons type-tag contents))
(define (type-tag datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))

(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))
(define (polar? z)
  (eq? (type-tag z) 'polar))

;; complex number representation rectangular (real, imaginary)
(define (real-part-rectangular z) (car z))
(define (imag-part-rectangular z (cdr z)))
(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))
(define (angle-rectangular z)
  (atan (imag-part-rectangular z)
        (real-part-rectangular z)))
(define (make-from-real-image-rectangular x y)
  (attach-tag 'rectangular (cons x y)))
(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular
              (cons (* r (cos a)) (* r (sin a)))))

;; complex number representation polar (magnitude, angle)
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))
(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))
(define (magnitude-polar z) (car z))
(define (angle-polar z) (cdr z))
(define (make-from-real-imag-polar x y)
  (attach-tag 'polar
              (cons (sqrt (+ (square x) (square y)))
                    (atan y x))))
(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (cons r a)))

;; Generic selectors

(define (real-part z)
  (cond ((rectangular? z)
         (real-part-rectangular (contents z)))
        ((polar? z)
         (real-part-polar (contents z)))
        (else (error "Unknown type -- REAL-PART" z))))
(define (imag-part z)
  (cond ((rectangular? z)
         (imag-part-rectangular (contents z)))
        ((polar? z)
         (imag-part-polar (contents z)))
        (else (error "Unknown type -- IMAG-PART" z))))
(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknow type -- MAGNITUDE" z))))
(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknow type -- ANGLE" z))))

(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))
(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))

;; Generic selectors redefined as
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; Constructors retrieved from the table
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))
(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
               var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(put 'deriv '+ 'make-sum)

;; The procedures are the same as before
(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

;;
(define (deriv-sum a1 a2)
  (make-sum (deriv a1 var)
            (deriv a2 var)))
(define (deriv-product m1 m2)
  (make-sum (make-product m1 (deriv m2 var))
            (make-product (deriv m1 var) m2)))

(put 'deriv '+ 'deriv-sum)
(put 'deriv '* 'deriv-product)

(define (deriv-expt base expt)
  (make-product (make-exponentiation (make-product (exponent exp) (base exp)) (- (exponent exp) 1))
                (deriv (base exp) var)))

(put 'deriv '** 'deriv-expt)

(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part)
           (* r (cos a)))
          ((eq? op 'imag-part)
           (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else (error "Unknown op -- MAKE-FROM-MAG-ANG" op)))))


(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generik 'sub x y))
(define (mul x y) (apply-generic 'muln x y))
(define (div x y) (apply-generik 'div x y))


(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))

  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))


(put 'magnitude '(complex) magnitude)

;; 2.78
(define (attach-tag type-tag contents)
  (if (number? contents)
      contents
      (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else  (error "Bad tagged datum -- TYPE-TAG" datum))))
(define (type-tag datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else  (error "Bad tagged datum -- CONTENTS" datum))))

;; 2.79
(define (equ? x y) (apply-generic 'equ? x y))

;; number package
(put 'equ? '(scheme-number scheme-number)
     (lambda (x y) (= x y)))

;; rational number package
(put 'equ? '(rational rational)
     (lambda (x y) (and (= (numer x) (numer y))
                        (= (denom x) (denom y)))))

;; complex number package
(put 'equ? '(complex complex)
     (lambda (z1 z2) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))

;; 2.80
(define (=zero? x) (apply-generic '=zero? x))

;; number package
(put '=zero? '(scheme-number)
     (lambda (x) (= 0 x)))

;; rational number package
(put '=zero? '(rational)
     (lambda (x) (and (= (numer x) 0)
                      (= (denom x) 0))))

;; complex number
(put '=zero? '(complex)
     (lambda (x) (and (= (real-part x) 0)
                      (= (imag-part x) 0))))


;; Coercion
(define (scheme-number->complex n)
  (make-complex-from-real-imag (contents n) 0))

;; place coercion methods into a coercion table
;; we assume a corresponding 'get-coercion' to retrieve the procedure
(put-coercion 'scheme-number 'complex scheme-number->complex)

;; apply-generic using coercion methods
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (let ((t1->t2 (get-coercion type1 type2))
                      (t2->t1 (get-coercion type2 type1)))
                  (cond ((eq? type1 type2) ;; this case should never be true because we would have found the procedure in the first 'get' call
                         (error "No method for these types" (list op type-tags)))
                        (t1->t2
                         (apply-generic op (t1->t2 a1) a2))
                        (t2->t1
                         (apply-generic op a1 (t2->t1 a2)))
                        (else
                         (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags)))))))


;; This tries only to coerce all types to the first arg
(define (apply-generic op . args)
  (define (coerce base arg)
    (let ((type1 (type-tag base))
          (type2 (type-tag arg)))
      (let ((t1->t2 (get-coercion type1 type2)))
        (if t1->t2
            (t1->t2 arg)
            (error "No coercion method for these types" (list type1 type2))))))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let ((coerced-args (map (lambda (t2) (coerce (car args) t2)) (cdr args))))
            (let ((coerced-type-tags (map type-tags coerced-args)))
              (let ((coerced-proc (get op coerced-type-tags)))
                (if coerced-proc
                    (apply proc (map contents coerced-args))
                    (error "No method for these types" (list op coerced-type-tags))))))))))


(define (raise arg tower)
  (if (null? tower)
      arg
      (if (eq? (type-tag arg) (car tower))
          (let ((supertype (cadr tower)))
            (let ((proc (get-coercion (type-tag arg) supertype)))
              (proc (contents arg))))
          (raise arg (cdr tower)))))


(define (lower? arg1 arg2 tower)
  (let ((type1 (type-tag arg1))
        (type2 (type-tag arg2)))
    (if (not (null? tower))
        (cond ((eq? type1 (car tower))
               true)
              ((eq? type2 (car tower))
               false)
              (else (lower arg1 arg2 (cdr tower))))
        (else (error "Types not in tower" (list type1 type2))))))


(define (apply-generic op . args)
  (let ((type-tags (map type-tag args))
        (tower '(scheme-number rational real complex)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (if (= (length args) 2)
              (let ((type1 (car type-tags))
                    (type2 (cadr type-tags))
                    (a1 (car args))
                    (a2 (cadr args)))
                (cond ((lower? a1 a2 tower)
                       (apply-generic op (raise a1 tower) a2))
                      ((lower? a2 a1 tower)
                       (apply-generic op a1 (raise a2 tower)))
                      (else
                       (error "No method for these types" (list op type-tags))))))
              (error "No method for these types" (list op type-tags))))))


;; Example: Symbolic Algebra
(define (add-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in same var -- ADD-POLY"
             (list p1 p2))))

(define (mul-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (mul-terms (term-list p1)
                            (term-list p2)))
      (error "Polys not in the same var -- MUL-POLY"
             (list p1 p2))))


(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list)
    (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in same var -- ADD-POLY"
               (list p1 p2))))

  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1)
                              (term-list p2)))
        (error "Polys not in the same var -- MUL-POLY"
               (list p1 p2))))

  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  'done)

(define add +)

(define (add-terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
         (let ((t1 (first-term L1)) (t2 (first-term L2)))
           (cond ((> (order t1) (order t2))
                  (adjoin-term
                   t1 (add-terms (rest-terms L1) L2)))
                 ((< (order t1) (order t2))
                  (adjoin-term
                   t2 (add-terms L1 (rest-terms L2))))
                 (else
                  (adjoin-term
                   (make-term (order t1)
                              (add (coeff t1) (coeff t2)))
                   (add-terms (rest-terms L1)
                              (rest-terms L2)))))))))

(define (mul-terms L1 L2)
  (if (empty-termlist? L1)
      (the-empty-termlist)
      (add-terms (mul-term-by-all-terms (first-term L1) L2)
                 (mul-terms (rest-terms L1) L2))))

(define (mul-term-by-all-terms t1 L)
  (if (empty-termlist? L)
      (the-empty-termlist)
      (let ((t2 (first-term L)))
        (adjoin-term
         (make-term (+ (order t1) (order t2))
                    (mul (coeff t1) (coeff t2)))
         (mul-term-by-all-terms t1 (rest-terms L))))))

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))
      term-list
      (cons term term-list)))

(define (the-empty-termlist) '())
(define (first-term term-list) (car term-list))
(define (rest-terms term-list) (cdr term-list))
(define (empty-termlist? term-list) (null? term-list))

(define (make-term order coeff) (list order coeff))
(define (order term) (car term))
(define (coeff term) (cadr term))

(define (make-polynomial var terms)
  ((get 'make 'polynomial) var terms))

;;
;; A coefficient is a multiplicative factor that can be either a number or a polynomial
(define (poly? term)
  (pair? term))

(define (=zero? ce)
  (cond ((poly? ce))
        ((number? ce) (= 0 ce))
        (else (error "Unknown type" ce))))

(put '=zero? '(polynomial)
     (lambda (coeff)
       (cond ((poly? coeff))
             ((number? coeff) (= 0 coeff))
             (else (error "Unknown type" coeff)))))

(define (negate p)
  (define (negate-terms terms)
    (if (empty-termlist? terms)
        the-empty-termlist
        (let ((first (first-term terms)))
          (cond ((poly? (coeff first))
                 (adjoin-term
                  (make-term (order first)
                             (negate (coeff first)))
                  (negate-terms (rest-terms terms))))
                (else
                 (adjoin-term
                  (make-term (order first)
                             (* -1 (coeff first)))
                  (negate-terms (rest-terms terms))))))))
  (make-poly (variable p) (negate-terms (term-list p))))

(define (sub-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
      (make-poly (variable p1)
                 (add-terms (term-list p1)
                            (term-list (negate p2))))
      (error "Polys not in the same var -- SUB-POLY" (list p1 p2))))

(define p1 (make-poly 'x (adjoin-term (make-term 2 3)
                                    (list (make-term 0 4)))))
(define p2 (make-poly 'x (adjoin-term (make-term 2 2)
                                      (list (make-term 0 2)))))


(define (same-variable? v1 v2)
  (and (symbol? 'x) (symbol 'x) (eq? 'x 'x)))
(sub-poly p1 p2)
