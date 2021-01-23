;; the following code implements a basic number generator that will yield a number to the-current-continuation, incremented by 'step' each time

(define (g step)
  (let ((kont #f)
        (n 0))
    (lambda ()
      (call/cc (lambda (k)
                 (set! kont k) ;; capture the continuation
                 (set! n (+ n step)) ;; increment n
                 (kont n)))))) ;; "yield" n to the continuation by applying the continuation to n

(define gen (g 3))
(while #t
  (format #t "val: ~s\n" (gen))
  (sleep 2))
