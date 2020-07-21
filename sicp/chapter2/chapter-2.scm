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
