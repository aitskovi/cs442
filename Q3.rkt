#lang racket

; Question 3
; Avi Itskovich, 20332164

(require test-engine/scheme-tests)

(define-struct posn (x y) #:transparent)

; unique-left: (listof any) -> (listof any)
; Generates a list containing only the unique elements in the
; passed in list. Maintains the leftmost occurence of an
; element.
(check-expect (unique-left empty) empty)
(check-expect (unique-left `(1 1)) `(1))
(check-expect (unique-left `(1 2 1)) `(1 2))
(check-expect (unique-left `(2 2 2 2 2 2 1)) `(2 1))
(define (unique-left lst) 
  (foldr 
   (lambda (fst rec) 
     (cons fst (filter (lambda (val) (not (equal? fst val))) rec)))
   empty
   lst))

; unique-right (listof any) -> (listof any)
; Generates a list containing only the unique elements in the
; passed in list. Maintains the rightmost occurence of an
; element.
(check-expect (unique-right empty) empty)
(check-expect (unique-right `(1 1)) `(1))
(check-expect (unique-right `(1 2 1)) `(2 1))
(check-expect (unique-right `(2 2 2 2 2 2 1)) `(2 1))
(define (unique-right lst) 
  (foldr (lambda (fst rec) (if (member fst rec) rec (cons fst rec))) empty lst))

; cross (listof any) (listof any) -> (listof (listof any))
; Produces the cross product of two lists.
(check-expect (cross empty empty) empty) 
(check-expect (cross `(a) empty) empty)
(check-expect (cross empty `(a)) empty)
(check-expect (cross `(a) `(1)) `((a 1)))
(check-expect (cross `(a b) `(1)) `((a 1) (b 1)))
(check-expect (cross `(a) `(1 2)) `((a 1) (a 2)))
(check-expect (cross `(a b) `(1 2)) `((a 1) (a 2) (b 1) (b 2)))
(define (cross lst1 lst2) 
  (foldr
    (lambda (fst rec)
      (append (map (lambda (val) (list fst val)) lst2) rec))
    empty
    lst1))

(test)
