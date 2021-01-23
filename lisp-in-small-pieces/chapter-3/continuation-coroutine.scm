;; The following procedures are coroutines that call each other as they iterate through the list of numbers, until no numbers remain
(define numbers '(1 2 3 4 5 6 7 8 9))

(define c1
  (lambda (kont)
    (if (pair? numbers)
        (let ((current (car numbers)))
          (set! numbers (cdr numbers)) ;; modify the global value of 'numbers'
          (format #t "c1 showing number ~s" current)
          (newline)
          (sleep 2)
          (c1 (call/cc kont)))) ;; calls (kont the-current-continuation), the-current-continuation in this case is just returning the symbol 'done
    'done))

(define c2
  (lambda (kont)
    (if (pair? numbers)
        (let ((current (car numbers)))
          (set! numbers (cdr numbers))
          (format #t "c2 showing number ~s" current)
          (newline)
          (sleep 2)
          (c2 (call/cc kont))))
    'finish))

(c1 c2)
