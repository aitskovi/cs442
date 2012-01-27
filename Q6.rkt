#lang racket

;;========================================
;; CS 442 Question 6
;; Avi Itskovich, 20332164
;; Based off of provided Q6 starter file.
;;========================================

(require test-engine/scheme-tests)

;; An expression is either [var] an alphabetic symbol, 
;; or [abs] (list 'λ v e) where v is an alphabetic symbol and e is an expression,
;; or [app] (list e1 e2) where e1 and e2 are expressions.

;; The following trivial functions make code that uses them more readable.
;; They have been completely implemented.

;; make-abs: symbol expression -> expression[abs]
(define (make-abs var expr) (list 'λ var expr))

;; var-of: expression[abs] -> symbol
(define var-of second)

;; body-of: expression[abs] -> expression
(define body-of third)

;; make-app: expression expression -> expression[app]
(define (make-app rator rand) (list rator rand))

;; rator-of: expression[app] -> expression
(define rator-of first)

;; rand-of: expression[app] -> expression
(define rand-of second)

;; abs?: expression -> Boolean
(define (abs? expr) 
  (and (list? expr) 
       (= 3 (length expr)) 
       (symbol=? 'λ (first expr))))

;; app?: expression -> Boolean
(define (app? expr) 
  (and (list? expr) 
       (= 2 (length expr))))

;; var?: expression -> Boolean
(define var? symbol?)

;; A set is a list of symbols without repetition.

;; The following helper functions are easy to write,
;;  and should be useful in implementing the interpreter.
;; They will not be directly tested by Marmoset.

;; add-to-set: symbol set -> set
(check-expect (add-to-set 'a empty) '(a))
(check-expect (add-to-set 'a '(c d)) '(a c d))
(check-expect (add-to-set 'a '(a b)) '(a b))
(define (add-to-set x s)
  (if (set-member? x s) s (cons x s)))

;; remove-from-set: symbol set -> set
(define (remove-from-set x s)
  (filter (lambda (val) (not (eq? x val))) s))

;; set-union: set set -> set
(check-expect (set-union empty empty) empty)
(check-expect (set-union '(a) empty) '(a))
(check-expect (set-union empty '(b)) '(b))
(check-expect (set-union '(a) '(a)) '(a))
(check-expect (set-union '(a b c d) '(e f a g c)) '(b d e f a g c))
(define (set-union s1 s2)
  (foldr (lambda (fst rec) (add-to-set fst rec)) s2 s1))

;; set-member? symbol set -> Boolean
(check-expect (set-member? 'a empty) #f)
(check-expect (set-member? 'a '(b)) #f)
(check-expect (set-member? 'a '(b a)) #t)
(define (set-member? x s)
  (if (findf (lambda (val) (eq? x val)) s) #t #f)) ; fill in

;; The following functions are directly tested by Marmoset.
;; In writing them, you may find the primitive function `gensym' useful.

;; alpha-equiv: expression expression -> Boolean
(check-expect (alpha-equiv 'x (make-abs 'x 'x)) #f)
(check-expect (alpha-equiv 'a 'b) #f)
(check-expect (alpha-equiv (make-abs 'x 'x) (make-abs 'y 'y)) #t)
(check-expect (alpha-equiv (make-abs 'x 'z) (make-abs 'w 'y)) #f)
(check-expect (alpha-equiv (make-abs 'x 'z) (make-abs'x 'y)) #f)
(check-expect (alpha-equiv (make-abs 'x (make-abs 'x 'x)) (make-abs 'y (make-abs 'y 'y))) #t)
(check-expect (alpha-equiv (make-app 'x (make-abs 'x 'x)) (make-app 'y (make-abs 'y 'y))) #f)
(check-expect (alpha-equiv (make-app 'x (make-abs 'x 'x)) (make-app 'x (make-abs 'y 'y))) #t)
(check-expect (alpha-equiv '(λ x (λ y z)) '(λ z (λ y z))) #f)
(check-expect (alpha-equiv '(λ z (λ y z)) '(λ x (λ y z))) #f)
(define (alpha-equiv e1 e2)
  (cond
    [(and (var? e1) (var? e2)) (eq? e1 e2)]
    [(and (abs? e1) (abs? e2)) 
     (if (eq? (var-of e1) (var-of e2))
         (alpha-equiv (body-of e1) (body-of e2))
         (if (free? (var-of e1) (body-of e2)) #f (alpha-equiv (body-of e1) (subst (var-of e2) (var-of e1) (body-of e2)))))]
    [(and (app? e1) (app? e2)) (and (alpha-equiv (rator-of e1) (rator-of e2))
                                    (alpha-equiv (rand-of e1) (rand-of e2)))]
    [else #f]))

;; subst: symbol expression expression -> expression
(define (subst var repl expr)
  (cond
    [(var? expr) (subst-var var repl expr)]
    [(abs? expr) (subst-abs var repl expr)]
    [(app? expr) (subst-app var repl expr)]
    [else (error "invalid expression")]))

(define (subst-var var repl expr)
  (if (eq? expr var) repl expr))

(define (subst-abs var repl expr)
  (local
    [(define body (body-of expr))
     (define param (var-of expr))]
    (cond
      [(eq? param var) expr]
      [(free? param repl)
       (local
         [(define param (gensym))
         (define body (subst (var-of expr) param (body-of expr)))]
         (make-abs param (subst var repl body)))]
      [else 
       (make-abs param (subst var repl body))])))

(define (subst-app var repl expr)
  (make-app (subst var repl (rator-of expr)) 
            (subst var repl (rand-of expr))))

;; free?: symbol expression -> boolean
;; Returns a boolean defining whether a variable is present and free in
;; the provided expression.
(check-expect (free? 'x 'x) #t)
(check-expect (free? 'x 'y) #f)
(check-expect (free? 'x '(λ x x)) #f)
(check-expect (free? 'x '(x (λ x x))) #t)
(check-expect (free? 'x '(λ y (y y))) #f)
(check-expect (free? 'x '(λ y (λ x y))) #f)
(define (free? var expr)
  (set-member? var (free expr)))
  ;(cond
  ;  [(var? expr) (if (eq? var expr) #t #f)]
  ;  [(app? expr) (or (free? var (rator-of expr))
  ;                    (free? var (rand-of expr)))]
  ;  [(abs? expr) (if (eq? (var-of expr) var) #f (free? var (body-of expr)))]))

(define (free expr)
  (cond
    [(var? expr) (add-to-set expr empty)]
    [(app? expr) (set-union (free (rator-of expr)) (free (rand-of expr)))]
    [(abs? expr) (remove-from-set (var-of expr) (free (body-of expr)))]))

;; reducible?: expression -> Boolean
(check-expect (reducible? 'a) #f)
(check-expect (reducible? (make-abs 'x 'x)) #f)
(check-expect (reducible? (make-app (make-abs 'x 'x) 'a)) #t)
(check-expect (reducible? (make-abs 'x (make-app (make-abs 'y 'y) 'z))) #t)
(check-expect (reducible? (make-abs 'x (make-abs 'y 'y))) #f)
(define (reducible? e)
  (cond
    [(var? e) #f]
    [(abs? e) (reducible? (body-of e))]
    [(app? e) (if (abs? (rator-of e)) #t (or (reducible? (rator-of e))
                                            (reducible? (rand-of e))))]))

;; reduce: expression -> expression
(check-expect (reduce '((λ x x) a)) 'a)
(check-expect (reduce '(λ x ((λ y y) z))) '(λ x z))
(check-expect (reduce '(x ((λ y y) z))) '(x z))
(check-expect (reduce '((((λ x x) z) ((λ x x) z)) ((λ x x) z)))
              '((z ((λ x x) z)) ((λ x x) z)))
(check-expect (reduce '((z ((λ x x) z)) ((λ x x) z)))
              '((z z) ((λ x x) z)))
(check-expect (reduce 'x) 'x)
(check-expect (reduce '((λ y ((λ x x) z)) w)) '((λ x x) z))
(check-expect (reduce '((λ x (x x)) y)) '(y y))
(define (reduce e)
  (cond
    [(var? e) e]
    [(app? e)
     (cond
       [(abs? (rator-of e))
        (local [(define fct (rator-of e))] (subst (var-of fct) (rand-of e) (body-of fct)))]
       [(reducible? (rator-of e)) (make-app (reduce (rator-of e)) (rand-of e))]
       [else (make-app (rator-of e) (reduce (rand-of e)))])]
    [(abs? e) (make-abs (var-of e) (reduce (body-of e)))]))

;; normalize: expression -> expression
(check-expect (alpha-equiv (normalize '((λ x (λ y (λ z x))) (λ x (x (y z)))))
                           '(λ g (λ r (λ x (x (y z))))))
              #t)
(check-expect (normalize '((λ y ((λ x x) z)) z)) 'z)
(check-expect (alpha-equiv (normalize '((λ x (λ z (λ y z))) z)) '(λ z (λ y z))) #t)
;(check-expect (normalize '((λ x (λ y x)) (y w)))
(check-expect (normalize 'z) 'z)
(check-expect (normalize '((((λ x (λ y (λ z (x (y z))))) (a (b c))) (d (e f))) (x (y z))))
              '((a (b c)) ((d (e f)) (x (y z)))))                                                                    
(define (normalize e)
  (if (reducible? e) (normalize (reduce e)) e))

;; bonus functions

;; parse: s-exp -> expression
(define (parse sexp)
  empty) ;; fill in

;; format-lc: expression -> string
(define (format-lc expr)
  empty)

(test)