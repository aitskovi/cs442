#lang racket

; Question 2
; Avi Itskovich, 20332164

(require test-engine/scheme-tests)

(define-struct posn (x y) #:transparent)

; in-triangle: posn posn posn posn -> boolean
; Checks if p is a point in the triangle defined by p1,p2,p3.
; This check is completed using the following algorithm:
;     The size of the 3 triangles with the vertex p, must be the
;     same size as the triangle p1,p2,p3. This fails when all the
;     vertices are stacked on top of each other so we also examine
;     this edge case.
(check-expect (in-triangle (make-posn 0 0) (make-posn 2 0) (make-posn 0 2) (make-posn 1 1)) true)
(check-expect (in-triangle (make-posn 0 0) (make-posn 2 0) (make-posn 0 2) (make-posn 3 3)) false)
(check-expect (in-triangle (make-posn 1 1) (make-posn 4 4) (make-posn 1 4) (make-posn 3 3)) true)
(check-expect (in-triangle (make-posn 1 1) (make-posn 4 4) (make-posn 1 4) (make-posn 1 1)) true)
(define (in-triangle p1 p2 p3 p)
  (cond
    [(and (equal? p1 p2) (equal? p2 p3)) (equal? p p1)]
    [(= (+ (abs (cross-prod (to-vector p p1) (to-vector p p2)))
           (abs (cross-prod (to-vector p p2) (to-vector p p3)))
           (abs (cross-prod (to-vector p p3) (to-vector p p1))))
        (abs (cross-prod (to-vector p1 p2) (to-vector p2 p3)))) true]
    [else false]))

; to-vector: posn posn -> posn
; Converts two points to a vector. Returns the value
; of the vector in a posn.
(check-expect (to-vector (make-posn 0 0) (make-posn 1 1)) (make-posn -1 -1)) 
(define (to-vector p1 p2)
  (make-posn (- (posn-x p1) (posn-x p2)) (- (posn-y p1) (posn-y p2))))

; cross-prod: posn posn -> number
; Computes the cross product of two vectors.
(check-expect (cross-prod (make-posn 1 1) (make-posn -1 -1)) 0)
(check-expect (cross-prod (make-posn 0 1) (make-posn 1 0)) -1)
(check-expect (cross-prod (make-posn 3 4) (make-posn 1 -2)) -10)
(define (cross-prod v1 v2)
  (- (* (posn-x v1) (posn-y v2)) (* (posn-y v1) (posn-x v2))))

(test)