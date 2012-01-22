#lang racket

;;===================================================
;; CS 442 Question 5
;; Avi Itskovich, 20332164
;; Note: All tests were defined in an external file.
;;===================================================

;; Data definitions for the various components of an FSM.
;; A token is some user-defined type depending on the application.
(provide make-trans)
(provide trans-state)
(provide trans-ctest)
(provide trans-action)
(provide trans-next)
(define-struct trans (state ctest action next) #:transparent)

;; scan: string transition-table symbol final-table -> (listof token)
;; A function that scans the input and generates tokens.
(provide scan)
(define (scan input transitions start finals)
  (scan-helper (string->list input) transitions start finals empty start))

;; scan-helper: (listof chars) transition-table symbol final-table (listof char) symbol -> (listof token)
;; A helper function for scanning the input and generating tokens.
(provide scan-helper)
(define (scan-helper input transitions state finals lexeme start)
  (local 
    [(define final (to-final state lexeme finals))] 
    (cond
      [(empty? input) (if final (list final) (error "unexpected end"))]
      [else (local [(define transition (get-transition state (first input) transitions))]
              (cond
                [(not transition) (if final
                                      (if (eq? state start)
                                          (error "infinite loop")
                                          (cons final (scan (list->string input) transitions start finals)))
                                      (error "no transition"))]
                [else (apply-transition input transitions state finals lexeme transition start)]))])))

;; apply-transition: (listof char) transition-table symbol final-table (listof char) trans symbol
;; Applies a specified transition.
(provide apply-transition)
(define (apply-transition input transitions state finals lexeme transition start)
  (cond
    [(eq? (trans-action transition) 'discard)
     (scan-helper (rest input) transitions (trans-next transition) finals lexeme start)]
    [(eq? (trans-action transition) 'shift)
     (scan-helper (rest input) transitions (trans-next transition) finals (cons (first input) lexeme) start)]
    [(eq? (trans-action transition) 'hold)
     (scan-helper input transitions (trans-next transition) finals lexeme start)]
    [else (error "incorrect trans-action")]))

;; get-transition: symbol char transition-table -> transition || #f
;; Returns the transition or false if no transition is found.
(provide get-transition)
(define (get-transition state input transitions)
  (findf (lambda (transition)
           (and (eq? state (trans-state transition))
                ((trans-ctest transition) input)))
         transitions))

;; to-final: symbol (listof chars) final-table -> any
;; Attempts to finalize a lexeme. If no final is possible, returns false.
(provide to-final)
(define (to-final state lexeme finals)
  (local 
    [(define final (filter (lambda (value) (eq? state (first value))) finals))]
    (cond
      [(empty? final) #f]
      [else ((second (first final)) (reverse lexeme))])))