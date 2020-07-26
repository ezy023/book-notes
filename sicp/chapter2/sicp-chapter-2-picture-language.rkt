#lang racket
(#%require sicp-pict)

;
;; Frames
;(define (frame-coord-map frame)
;  (lambda (v)
;    (add-vect
;     (origin-frame frame)
;     (add-vect (scale-vect (xcor-vect v)
;                           (edge1-frame frame))
;              (scale-vect (ycor-vect v)
;                           (edge2-frame frame))))))

;; Ex 2.46
;(define (my-make-vect x y)
;  (cons x y))
;(define (xcor-vect v) (car v))
;(define (ycor-vect v) (cdr v))
;
;(define (add-vect v1 v2)
;  (make-vect (+ (xcor-vect v1)
;                (xcor-vect v2))
;             (+ (ycor-vect v1)
;                (ycor-vect v2))))
;
;(define (sub-vect v1 v2)
;  (make-vect (- (xcor-vect v1)
;                (xcor-vect v2))
;             (- (ycor-vect v1)
;                (ycor-vect v2))))
;
;(define (scale-vect s v)
;  (make-vect (* s (xcor-vect v))
;             (* s (ycor-vect v))))
;

;; Ex 2.47
;; a
;(define (make-frame-a origin edge1 edge2)
;  (list origin edge1 edge2))
;
;(define (origin-frame-a frame)
;  (car frame))
;(define (edge1-frame-a frame)
;  (cadr frame))
;(define (edge2-frame-a frame)
;  (caddr frame))
;
;(define test-a (make-frame-a 1 2 3))
;(origin-frame-a test-a)
;(edge1-frame-a test-a)
;(edge2-frame-a test-a)
;
;;; b
;(define (make-frame-b origin edge1 edge2)
;  (cons origin (cons edge1 edge2)))
;
;(define (origin-frame-b frame)
;  (car frame))
;(define (edge1-frame-b frame)

;  (cadr frame))
;(define (edge2-frame-b frame)
;  (cddr frame))
;
;(define test-b (make-frame-b 1 2 3))
;(origin-frame-b test-b)
;(edge1-frame-b test-b)
;(edge2-frame-b test-b)

;; Painters
;(define (segments->painter segment-list)
;  (lambda (frame)
;    (for-each
;     (lambda (segment)
;       (draw-line
;        ((frame-coord-map frame) (start-segment segment))
;        ((frame-coord-map frame) (end-segment segment))))
;     segment-list)))

;; Ex 2.48
;(define (make-segment-2 sv ev)
;  (cons sv ev))
;(define (start-segment-2 segment)
;  (car segment))
;(define (end-segment-2 segment)
;  (cdr segment))

;; Ex 2.49
;a
(define outline-painter
  (lambda (frame)
    (segments->painter (list (make-segment (frame-origin frame) (frame-edge1 frame))
                             (make-segment (frame-origin frame) (frame-edge2 frame))
                             (make-segment (frame-edge2 frame) (vector-add (frame-edge1 frame) (frame-edge2 frame)))
                             (make-segment (frame-edge1 frame) (vector-add (frame-edge2 frame) (frame-edge1 frame)))))))

(define test-frame (make-frame (make-vect 0 0)
                               (make-vect 0.99 0)
                               (make-vect 0 0.99)))
(paint (outline-painter test-frame))

;b
(define x-painter
  (lambda (frame)
    (segments->painter (list (make-segment (frame-origin frame) (vector-add (frame-edge1 frame) (frame-edge2 frame)))
                             (make-segment (frame-edge1 frame) (frame-edge2 frame))))))

(paint (x-painter test-frame))

;c

(define diamond-painter
  (lambda (frame)
    (segments->painter (list (make-segment (vector-scale 0.5 (frame-edge1 frame)) (vector-scale 0.5 (frame-edge2 frame)))
                             (make-segment (vector-scale 0.5 (frame-edge1 frame)) (vector-add (make-vect 1 0) (vector-scale 0.5 (frame-edge2 frame))))
                             (make-segment (vector-add (make-vect 1 0) (vector-scale 0.5 (frame-edge2 frame))) (vector-add (make-vect 0 1) (vector-scale 0.5 (frame-edge1 frame))))
                             (make-segment (vector-add (make-vect 0 1) (vector-scale 0.5 (frame-edge1 frame))) (vector-scale 0.5 (frame-edge2 frame)))
                             ))))

(paint (diamond-painter test-frame))


;; Transforming and combining painters
;(define (my-frame-coord-map frame)
;  (lambda (v)
;    (vector-add
;     (origin-frame frame)
;     (add-vect (scale-vect (xcor-vect v)
;                           (edge1-frame frame))
;              (scale-vect (ycor-vect v)
;                           (edge2-frame frame))))))
;(define (my-transform-painter painter origin corner1 corner2)
;  (lambda (frame)
;    (let ((m (my-frame-coord-map frame)))
;      (let ((new-origin (m origin)))
;        (painter
;         (make-frame new-origin
;                     (vector-sub (m corner1) new-origin)
;                     (vector-sub (m corner2) new-origin)))))))
;
;(define (my-flip-vert painter)
;  (my-transform-painter painter
;                        (make-vect 0.0 1.0)    ; new origin
;                        (make-vect 1.0 1.0)    ; new end of edge1
;                        (make-vect 0.0 0.0)))  ; new end of edge2
;
;(define (shrink-to-upper-right painter)
;  (my-transform-painter painter
;                        (make-vect 0.5 0.5)
;                        (make-vect 1.0 0.5)
;                        (make-vect 0.5 1.0)))
;
;(define (beside painter1 painter2)
;  (let ((split-point (make-vect 0.5 0.0)))
;    (let ((paint-left
;           (transform-painter painter1
;                              (make-vect 0.0 0.0)
;                              split-point
;                              (make-vect 0.0 1.0)))
;          (paint-right
;           (transform-painter painter2
;                              split-point
;                              (make-vect 1.0 0.0)
;                              (make-vect 0.5 1.0))))
;      (lambda (frame)
;        (paint-left frame)
;        (paint-right frame)))))

(define (my-frame-coord-map frame)
  (lambda (v)
    (vector-add
     (frame-edge1 frame)
     (vector-add (vector-scale (vector-xcor v)
                           (frame-edge1 frame))
              (vector-scale (vector-ycor v)
                           (frame-edge2 frame))))))

(define (my-transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (my-frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter
         (make-frame new-origin
                     (vector-sub (m corner1) new-origin)
                     (vector-sub (m corner2) new-origin)))))))


; Exercise 2.50
(define (my-flip-horiz painter)
  (my-transform-painter painter
                        (make-vect 1.0 0.0)
                        (make-vect 0.0 0.0)
                        (make-vect 1.0 1.0)))

()
(paint (my-flip-horiz einstein))
