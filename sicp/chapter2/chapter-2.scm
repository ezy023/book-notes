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
