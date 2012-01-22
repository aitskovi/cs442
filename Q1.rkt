#lang racket
; Question 1
; Avi Itskovich, 20332164

(require test-engine/scheme-tests)

; perms: (listof any) -> (listof lists)
; Creates a list of all possible permutations of the
; passed in values.
(check-expect (perms empty) `(()))
(check-expect (perms `(1)) `((1)))
(check-expect (perms `(1 2)) `((1 2) (2 1)))
(check-expect (perms `(1 2 3)) `((1 2 3) (2 1 3) (2 3 1) (1 3 2) (3 1 2) (3 2 1)))
(define (perms values)
  (cond
    [(empty? values) (list empty)]
    [else (permute-all (first values) (perms (rest values)))]))

; permute-all: any (listof lists) -> (listof lists)
; Create a new set of permutations by adding another element to the set.
; Permutes all the old instances with the new element.
(check-expect (permute-all 1 (list empty)) `((1)))
(define (permute-all value lists)
  (cond
    [(empty? lists) empty]
    [else (append (permute value (first lists)) (permute-all value (rest lists)))]))

; permute: any (listof any) -> (listof lists)
; Takes a list and generates all permutations of it and another element.
(check-expect (permute 1 empty) `((1)))
(check-expect (permute 1 `(2)) `((1 2) (2 1)))
(check-expect (permute 1 `(2 3)) `((1 2 3) (2 1 3) (2 3 1)))
(define (permute value values)
  (cond
    [(empty? values) (list (list value))]
    [else  (cons (cons value values)
                 (cons-all (first values) (permute value (rest values))))]))

; cons-all: insert any (listof lists) -> (listof lists)
; Inserts a value at the front of every list.
(check-expect (cons-all 1 empty) empty)
(check-expect (cons-all 1 `()) `())
(check-expect (cons-all 1 `((2 3))) `((1 2 3)))
(check-expect (cons-all 1 `((2 3) (3 4))) `((1 2 3) (1 3 4)))
(define (cons-all value values)
  (cond
    [(empty? values) empty]
    [else (cons (cons value (first values)) (cons-all value (rest values)))]))

(test)
