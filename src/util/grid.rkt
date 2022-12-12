#lang racket

(provide (all-defined-out))

(struct grid (data w h))

(define (make-grid w h [v 0])
  (grid (make-vector (* w h) v)
        w h))

(define (grid-map f g)
  (grid (vector-map f (grid-data g))
        (grid-w g)
        (grid-h g)))

(define (in-grid-coordinates g)
  (for*/stream ([j (in-range (grid-h g))]
                [i (in-range (grid-w g))])
    (cons i j)))

(define (grid-imap f g)
  (grid
   (for/vector ([c (in-grid-coordinates g)])
     (define-values (i j) (values (car c) (cdr c)))
     (define v (grid-ref g i j))
     (f v i j))
   (grid-w g)
   (grid-h g)))

(define (grid-valid-indices? g i j)
  (and
   (integer? i)
   (integer? j)
   (< -1 i (grid-w g))
   (< -1 j (grid-h g))))

(define (grid-index-at g i j)
  (+ (* j (grid-w g)) i))
(define (grid-set! g i j v)
  (vector-set! (grid-data g)
               (grid-index-at g i j)
               v))
(define (grid-ref g i j)
  (vector-ref (grid-data g)
              (grid-index-at g i j)))

(define (grid-dump g [elem->string ~a])
  (for ([j (in-range (grid-h g))])
    (for ([i (in-range (grid-w g))])
      (display (elem->string (grid-ref g i j))))
    (display #\newline)))
