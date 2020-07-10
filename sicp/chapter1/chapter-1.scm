(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (/ (abs (- guess (improve guess x)))
        guess)
     0.00001))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))

(sqrt-iter 1.0 0.001)
(sqrt-iter 1.0 81)

(sqrt-iter 100 1000000000000)

(/ 0.27 1.5)



(define (cube-improve guess x)
  (/ (+ (/ x (square guess))
        (* 2 guess))
     3))

(define (cube-good-enough? guess x)
  (< (/ (abs (- guess (cube-improve guess x)))
        guess)
     0.00001))

(define (cube-rt guess x)
  (if (cube-good-enough? guess x)
      guess
      (cube-rt (cube-improve guess x) x)))

(cube-rt 1.0 27)
(cube-rt 1.0 125)
(cube-rt 1.0 (* 12 12 12))

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(inc (+ 2 5))

(+ 4 5)
(+ (inc (+ 3 5)))
(+ (inc (+ (inc (+ 2 5)))))
(+ (inc (+ (inc (+ (inc (+ 1 5)))))))
(+ (inc (+ (inc (+ (inc (+ (inc (+ 0 5)))))))))
(+ (inc (+ (inc (+ (inc (+ (inc 5))))))))
(+ (inc (+ (inc (+ (inc 6))))))

(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9


(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* 2 (f (- n 2)))
         (* 3 (f (- n 3))))))
(f 5)

(define (f n)
  (if (< n 3)
      n
      ))


;; what rule defines state transitions?
a <- a + 2b + 3c
b <- a
c <- b
(define (fi n)
  (define (f-iter a b c count)
    (cond ((< n 3) n)
          ((<= count 0) a)
          (else (f-iter (+ a (* 2 b) (* 3 c))
                        a
                        b
                        (- count 1)))))
  (f-iter 2 1 0 (- n 2)))

(fi 1)


(define (pascal row col)
  (cond ((or (= col 1) (= col row))
         1)
        (else (+ (pascal (- row 1) col)
                 (pascal (- row 1) (- col 1))))))

(pascal 8 5)


(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(fast-expt 5 6)

(define (square x) (* x x))
(define (even? n)
  (= (remainder n 2) 0))
(define (fast-expt-iter base exp)
 (define (expt-iter b n acc)
   (cond ((= n 0) acc)
         ((even? n) (expt-iter b (/ n 2) (* acc (square b))))
         (else (expt-iter b (- n 1) (* acc b)))))
 (expt-iter base exp 1))

(fast-expt-iter 5 6)


(define (even? n)
  (= (remainder n 2) 0))
(define (double x) (+ x x))
(define (halve x) (/ x 2))
(define (fast-mult a b)
  (cond ((= b 0) 0)
        ((= b 1) a)
        ((even? b) (fast-mult (double a) (halve b)))
        (else (+ a (fast-mult a (- b 1))))))

(fast-mult 2 10)
(fast-mult 5 5)
(fast-mult 12 12)

(define (fast-mult-iter x y)
  (define (mult-iter a b acc)
    (cond ((= b 0) acc)
          ((even? b) (mult-iter (double a) (halve b) acc))
          (else (mult-iter a (- b 1) (+ acc a)))))
  (mult-iter x y 0))

(fast-mult-iter 2 10)
(fast-mult-iter 3 4)

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a b -1 3 (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(smallest-divisor 245)

(smallest-divisor 19999)


(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

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

(define (search-for-primes n count)
  (cond ((= count 3) (newline) (display "DONE"))
        ((even? n) (search-for-primes (+ n 1) count))
        ((prime? n)
         (search-for-primes (+ n 1) (+ count 1)))
        (else (search-for-primes (+ n 1) count))))

(search-for-primes 1002 0)

(prime? 1005)


(define (smallest-divisor n)
  (find-divisor n 2))

(define (next divisor)
  (if (= divisor 2)
      3
      (+ divisor 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))




(define (timed-prime-test n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

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


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (congruent-check n)
  (define (check-num a)
    (display a)
    (newline)
    (cond ((>= a n) #t)
          ((= (remainder (fast-expt a n) n) (remainder a n))
           (check-num (+ a 1)))
          (else #f)))
  (check-num 1))

(congruent-check 561)
(congruent-check 1105)
(congruent-check 1729)
(congruent-check 2465)
(congruent-check 2821)
(congruent-check 6601)
(congruent-check 100)



(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (cube x)
  (* x x x))

(define (simpson-integral f a b n)
  (define h (/ (- b a) n))
  (define (y k) (+ a (* k h)))
  (define (coef k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k)
    (* (coef k) (f (y k))))
  (define (next k) (+ k 1))
  (* (/ h 3) (sum-iter term 0 next n)))

(simpson-integral cube 0.0 1.0 100)
(simpson-integral cube 0.0 1.0 1000)

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))




(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))


(define (pi-prod)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (define (next n) (+ n 1))
  (* 4 (product term 1.0 next 10000)))

(pi-prod)

(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))



(define (pi-prod-iter)
  (define (term n)
    (if (even? n)
        (/ (+ n 2) (+ n 1))
        (/ (+ n 1) (+ n 2))))
  (define (next n) (+ n 1))
  (* 4 (product-iter term 1.0 next 1000)))

(pi-prod-iter)


(define (accumulate combiner null-value term a next b)
  (define (acc-iter a result)
    (if (> a b)
        result
        (acc-iter (next a) (combiner (term a) result))))
  (acc-iter a null-value))


(define (identity x) x)
(define (inc x) (+ x 1))

(define (sum-acc a b)
  (accumulate + 0 identity a inc b))

(sum-acc 1 5)
;; 15

(define (prod-acc a b)
  (accumulate * 1 identity a inc b))

(prod-acc 1 5)
;; 120


(define (filtered-accumulate combiner null-value term a next b predicate?)
  (cond ((> a b) null-value)
        ((predicate? a)
         (combiner (term a)
                   (filtered-accumulate combiner null-value term (next a) next b predicate?)))
        (else (filtered-accumulate combiner null-value term (next a) next b predicate?))))

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

(define (square x) (* x x))
(define (sum-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))
(sum-prime-squares 2 5)


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (relatively-prime? i n)
  (= (gcd i n) 1))

(define (product-relative-primes n)
  (define (filter i)
    (relatively-prime? i n))
  (filtered-accumulate * 1 identity 1 inc n filter))
(product-relative-primes 10)


(define (f g)
  (g 2))

(f f)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define tolerance 0.0001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(fixed-point (lambda (x) (/ (log 1000) (log x)))
             2)
;Value: 4.555563237292884
;; 29 steps

(define (average x y)
  (/ (+ x y) 2))
(fixed-point (lambda (x) (average x (/ (log 1000) (log x))))
             2)
;Value: 4.5555465521473675
;; 8 steps

(define (cont-frac n d k)
  (if (= 0 k)
      (/ (n k)
         (d k))
      (/ (n k)
         (+ (d k) (cont-frac n d (- k 1))))))

(/ 1
   (cont-frac (lambda (x) 1.0)
            (lambda (x) 1.0)
            11))

(define (cont-frac-iter n d k result)
  (if (= 0 k)
      result
      (/ (n k)
         (+ (d k) (cont-frac-iter n d (- k 1) (/ (n k) (d k)))))))

(/ 1
   (cont-frac-iter (lambda (x) 1.0)
                 (lambda (x) 1.0)
                 20
                 1))


(define (cont-frac-iter n d k)
  (define (iter result i)
    (if (= i 0)
        result
        (iter (/ (n i) (+ (d i) result))
                       (- i 1))))
  (iter (/ (n k) (d k)) (- k 1)))

(/ 1
   (cont-frac-iter (lambda (x) 1.0)
                   (lambda (x) 1.0)
                   15))

(define (cont-frac n d k)
  (define (frac i)
    (if (< i k)
        (/ (n i) (+ (d i) (frac (+ i 1))))
        (/ (n i) (d i))))
  (frac 1))

(/ 1
   (cont-frac (lambda (x) 1.0)
            (lambda (x) 1.0)
            15))

(define (calc-e)
  (+ 2
     (cont-frac-iter (lambda (x) 1.0)
                (lambda (x)
                  (if (not (= 0 (remainder (+ x 1) 3)))
                      1
                      (* 2 (/ (+ x 1) 3))))
                20)))
(calc-e)

 (define (e-euler k)
   (+ 2.0 (cont-frac (lambda (i) 1)
                     (lambda (i)
                       (if (= (remainder i 3) 2)
                           (/ (+ i 1) 1.5)
                           1))
                     k)))





(tan-cf 10.0 40)
(tan 10)

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))

((deriv cube) 5)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))


(define a 1)
(define b 2)
(define c 3)
(newtons-method (cubic 4 -8 7) 1)

(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))


(define (inc x) (+ x 1))

(define (double g)
  (lambda (x) (g (g x))))

((double inc) 1)

(((double (double double)) inc) 5)

(define (compose f g)
  (lambda (x) (f (g x))))

((compose square inc) 6)

(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))

((repeated square 2) 5)

(define dx 0.00001)
(define (average3 a b c)
  (/ (+ a b c) 3))
(define (smooth f)
  (lambda (x)
    (average3 (f (- x dx))
              (f x)
              (f (+ x dx)))))

(define (n-fold-smooth f n)
  (repeated (smooth f) n))

(define (iterative-improve good-enough? improve)
  (define (iter-imp guess)
    (if (good-enough? guess)
        guess
        (iter-imp (improve guess))))
  iter-imp)


(define (sqrt-ii x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.0001))
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2))
  ((iterative-improve good-enough? improve) 1.0))

(sqrt-ii 16)

(define (fixed-point-ii f first-guess)
  (define (good-enough? guess)
    (< (abs (- guess (f guess))) 0.0001))
  (define (improve guess)
    (f guess))
  ((iterative-improve good-enough? improve) first-guess))

(fixed-point-ii (lambda (x) (/ (log 1000) (log x)))
                2)

(fixed-point-ii (lambda (x) (+ 1 (/ 1 x))) 2.0)
