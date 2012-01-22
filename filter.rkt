#lang racket

(require test-engine/scheme-tests)

; filter-fold: (any -> boolean) (listof any) -> (listof any)
; Implementation of filter in terms of foldr.
(check-expect (filter-fold (lambda (x) true) `(1 2 3 4)) `(1 2 3 4))
(check-expect (filter-fold (lambda (x) true) empty) empty)
(check-expect (filter-fold (lambda (x) (> x 2)) `(1 2 3 2 4)) `(3 4))
(define (filter-fold pred values)
  (foldr (lambda (fst rec) (if (pred fst) (cons fst rec) rec)) empty values))

(test)