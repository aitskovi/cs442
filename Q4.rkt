#lang racket

; Question 4
; Avi Itskovich, 20332164

(require test-engine/scheme-tests)

; Note: To make adding and removing nodes easier, we will define the right,
; to the be either 1 smaller or equal to the one on the left. We will
; maintain this by flipping the two on every addition and subtraction.
; Always add to the right bh and subtract from left bh.
(define-struct bhnode (key left right) #:transparent)

; find-min: bh -> any
; Returns the minimum key in the BH.
(check-expect (find-min empty) empty)
(check-expect (find-min (make-bhnode 1 empty empty)) 1)
(check-expect (find-min (make-bhnode 1 (make-bhnode 3 empty empty) empty)) 1)
(check-expect (find-min (make-bhnode 1 empty (make-bhnode 3 empty empty))) 1)
(check-expect (find-min (make-bhnode 1
                                     (make-bhnode 3 empty empty)
                                     (make-bhnode 4 empty empty))) 1)
(define (find-min bh)
  (cond
    [(empty? bh) empty]
    [else (bhnode-key bh)]))

; add: any bh -> bh
; Returns a bh with the key added to it.
(check-expect (add 1 empty) (make-bhnode 1 empty empty))
(check-expect (add 1 (make-bhnode 2 empty empty))
              (make-bhnode 1 (make-bhnode 2 empty empty) empty))
(check-expect (add 1 (make-bhnode 2 (make-bhnode 3 empty empty) empty))
              (make-bhnode 1 (make-bhnode 2 empty empty) (make-bhnode 3 empty empty)))
(define (add k bh)
  (cond
    [(empty? bh) (make-bhnode k empty empty)]
    [(< k (bhnode-key bh)) (add (bhnode-key bh)
                                (make-bhnode k (bhnode-left bh) (bhnode-right bh)))]
    [else (make-bhnode (bhnode-key bh) (add k (bhnode-right bh)) (bhnode-left bh))]))

; delete-min: bh -> bh
; Returns a bh, without the minimum key.
;(check-expect (delete-min empty) empty)
(check-expect (delete-min empty) empty)
(check-expect (delete-min (make-bhnode 1 empty empty)) empty)
(check-expect (delete-min (make-bhnode 1 (make-bhnode 2 empty empty) (make-bhnode 3 empty empty)))
              (make-bhnode 2 (make-bhnode 3 empty empty) empty))
(define (delete-min bh)
  (cond
    [(empty? bh) empty]
    [else (merge (bhnode-left bh) (bhnode-right bh))]))

; delete-one: bh -> (list any bh)
; Removes a value from the bh.
; Returns a list with the removed value and the new bh.
(check-expect (delete-one empty) (list empty empty))
(check-expect (delete-one (make-bhnode 1 empty empty)) (list 1 empty))
(check-expect (delete-one (make-bhnode 1
                                  (make-bhnode 3 empty empty)
                                  (make-bhnode 4 empty empty)))
              (list 3 (make-bhnode 1 (make-bhnode 4 empty empty) empty)))
(define (delete-one bh)
  (cond
    [(empty? bh) (list empty empty)]
    [(empty? (bhnode-left bh)) (list (bhnode-key bh) empty)]
    [else (local [(define vals (delete-one (bhnode-left bh)))]
            (list (first vals) (make-bhnode (bhnode-key bh) (bhnode-right bh) (second vals))))]))

; merge: bh bh -> bh
; Merges two bh's and returns the result.
(check-expect (merge empty empty) empty)
(check-expect (merge (make-bhnode 1 empty empty) empty) (make-bhnode 1 empty empty))
(check-expect (merge empty (make-bhnode 1 empty empty)) (make-bhnode 1 empty empty))
(check-expect (merge (make-bhnode 1 empty empty) (make-bhnode 2 empty empty))
              (make-bhnode 1 (make-bhnode 2 empty empty) empty))
(check-expect (merge (make-bhnode 2 empty empty) (make-bhnode 1 empty empty))
              (make-bhnode 1 (make-bhnode 2 empty empty) empty))
(check-expect (merge (make-bhnode 1
                                  (make-bhnode 3 empty empty)
                                  (make-bhnode 4 empty empty))
                     (make-bhnode 2 empty empty))
              (make-bhnode 1
                           (make-bhnode 2 (make-bhnode 4 empty empty) empty)
                           (make-bhnode 3 empty empty)))
(check-expect (merge (make-bhnode 2 empty empty)
                     (make-bhnode 1
                                  (make-bhnode 3 empty empty)
                                  (make-bhnode 4 empty empty)))
              (make-bhnode 1
                           (make-bhnode 2 (make-bhnode 3 empty empty) empty)
                           (make-bhnode 4 empty empty)))
(define (merge bh1 bh2)
  (cond
    [(empty? bh1) bh2]
    [(empty? bh2) bh1]
    [else
     (local [(define vals (delete-one bh2))]
       (merge (add (first vals) bh1) (second vals)))]))

(test)
