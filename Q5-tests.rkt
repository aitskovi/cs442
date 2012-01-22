#lang racket

;; CS 442 Question 5 Tests
;; Avi Itskovich, 20332164

(require test-engine/scheme-tests)
(require "Q5.rkt")

(define (chartest ch) (lambda (x) (char=? x ch)))

(define sample-trans-table
  (list
   (make-trans 'start char-whitespace? 'discard 'start)
   (make-trans 'start (chartest #\c) 'discard 'first)
   (make-trans 'first (chartest #\a) 'shift 'rest)
   (make-trans 'first (chartest #\d) 'shift 'rest)
   (make-trans 'rest (chartest #\a) 'shift 'rest)
   (make-trans 'rest (chartest #\d) 'shift 'rest)
   (make-trans 'rest (chartest #\r) 'discard 'end)))

(define sample-final-table
  (list
   (list 'end list->string)
   (list 'start list->string)))


;; to-final tests
(check-expect (to-final 'first '(a b c) sample-final-table) #f)
(check-expect (to-final 'end (list #\r #\a #\c) sample-final-table) "car")

;; scan tests
(check-expect (scan "car cdr cadaddr" sample-trans-table 'start sample-final-table) '("a" "d" "adadd"))
(check-error (scan "a" sample-trans-table 'start sample-final-table) "infinite loop")
(test)
