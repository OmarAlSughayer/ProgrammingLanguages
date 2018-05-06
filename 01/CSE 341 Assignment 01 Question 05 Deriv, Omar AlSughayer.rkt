#lang racket

;Omar Adel AlSughayer (1337255)
;CSE 341 AB
;Assignment 1 - Question 5

;;; CSE 341 - Racket
;;; A classic example of Scheme/Racket programs as data: symbolic differentiation.  Adapted from 
;;; "Structure and Interpretation of Computer Programs" Chapter 2.  Somewhat modified: doesn't use
;;; constructor functions, and it separates out finding the derivative from simplifying expressions.   
;;; Includes unit tests.

;; the top-level function deriv takes an expression and a variable, 
;; and returns the derivative of that expression with respect to the variable,
;; in simplified form
(define (deriv exp var)
  (simplify (basic-deriv exp var)))

;; basic-deriv takes the derivative of an expression exp with respect to a variable and
;; return the result without simplification
(define (basic-deriv exp var)
  (cond [(number? exp) 0]
        [(symbol? exp)
         (if (eq? exp var) 1 0)]
        [(sum? exp)
         (list '+ (basic-deriv (left exp) var) (basic-deriv (right exp) var))]
        [(minus? exp)
         (list '- (basic-deriv (left exp) var) (basic-deriv (right exp) var))]
        [(sin? exp)
         (list '* (list 'cos (left exp)) (basic-deriv (left exp) var)) ]
        [(cos? exp)
         (list '- 0 (list '* (list 'sin (left exp)) (basic-deriv (left exp) var)))]
        [(product? exp)
         (list '+
          (list '* (left exp) (basic-deriv (right exp) var))
          (list '* (basic-deriv (left exp) var) (right exp)))]
        [(expt? exp)
         (list '* (list '* (right exp) (basic-deriv (left exp) var)) (list 'expt (left exp) (list '- (right exp) 1)))]
        [else (error "unknown expression type -- basic-deriv" exp)]))

;; predicates and access functions

;; test whether a list structure is a sum
(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

;; test weather a list structure is a minus operator
(define (minus? x)
  (and (pair? x) (eq? (car x) '-)))

;; test whether a list structure is a product
(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

;; test whether a list structure is a sin
(define (sin? x)
  (and (pair? x) (equal? (car x) 'sin)))

;; test whether a list structure is a cos
(define (cos? x)
  (and (pair? x) (eq? (car x) 'cos)))

;; test whether a list structure is an expt
(define (expt? x)
  (and (pair? x) (eq? (car x) 'expt)))

;; get the left hand part of a sum or product
(define (left exp)
  (cadr exp))
;; get the right hand part of a sum or product
(define (right exp)
  (caddr exp))

;; basic simplification function (nothing too fancy ... doesn't know about commutativity or associativity)
(define (simplify exp)
  (cond [(sum? exp) (simplify-sum exp)]
        [(minus? exp) (simplify-minus exp)]
        [(product? exp) (simplify-product exp)]
        [(sin? exp) (simplify-sin exp)]
        [(cos? exp) (simplify-cos exp)]
        [(expt? exp) (simplify-expt exp)]
        ;; if we get here, we can't simplify exp
        [else exp]))

; simplifies a sum expression
(define (simplify-sum exp)
  ;; to simplify a sum, we need to first recursively simplify the left and right parts
  (let ([a (simplify (left exp))]
        [b (simplify (right exp))])
    (cond [(equal? 0 a) b]
          [(equal? 0 b) a]
          [(and (number? a) (number? b)) (+ a b)]
          [else (list '+ a b)])))

;simplifies a sin expression
(define (simplify-sin exp)
  ;; to simplify a cos, we need to first recursively simplify what's inside the trig function
  (let ([a (simplify (left exp))])
    (cond [(number? a) (sin a)]
          [else (list 'sin a)])))

;simplifies a cos expression
(define (simplify-cos exp)
  ;; to simplify a cos, we need to first recursively simplify what's inside the trig function
  (let ([a (simplify (left exp))])
    (cond [(number? a) (cos a)]
          [else (list 'cos a)])))

;simplifies an expt expression
(define (simplify-expt exp)
  ;; to simplify an expt, we need to first recursively simplify the left and right parts
  (let ([num (simplify (left exp))]
        [pow (simplify (right exp))])
    (cond [(equal? 0 pow) 1]
          [(equal? 1 pow) num]
          [(and (number? num) (number? pow)) (expt num pow)]
          [else (list 'expt num pow)])))

;simplifies a minus expression
(define (simplify-minus exp)
  ;; to simplify a minus, we need to first recursively simplify the left and right parts
  (let ([a (simplify (left exp))]
        [b (simplify (right exp))])
    (cond [(and (equal? 0 a) (number? b)) (* -1 b)]
          [(equal? 0 b) a]
          [(and (number? a) (number? b)) (- a b)]
          [else (list '- a b)])))

;simplifies a product expression
(define (simplify-product exp) 
  (let ([a (simplify (left exp))]
        [b (simplify (right exp))])
    (cond [(or (equal? 0 a) (equal? 0 b)) 0]
          [(equal? 1 a) b]
          [(equal? 1 b) a]
          [(and (number? a) (number? b)) (* a b)]
          [else (list '* a b)])))
           
;; Unit tests.  See
;; http://docs.racket-lang.org/rackunit/quick-start.html
;; for documentation on Racket's unit testing library.
(require rackunit)

(define deriv-tests 
  (test-suite 
   "tests for deriv program"
   (check-equal? (deriv 'x 'x) 1 "deriv of x wrt x")
   (check-equal? (deriv 'y 'x) 0 "deriv of y wrt x")
   (check-equal? (deriv '(+ x 3) 'x) 1 "deriv of (+ x 3) wrt x")
   (check-equal? (deriv '(- x 3) 'x) 1 "deriv of (- x 3) wrt x")
   (check-equal? (deriv '(- 3 x) 'x) -1 "deriv of (- 3 x) wrt x")
   (check-equal? (deriv '(* (+ 2 3) x) 'x) 5 "deriv of unsimplified expression")
   (check-equal? (deriv '(* (- 2 3) x) 'x) -1 "deriv of unsimplified expression")
   (check-equal? (deriv '(+ x y) 'x) 1 "deriv of (+ x y) wrt x")
   (check-equal? (deriv '(- x y) 'x) 1 "deriv of (- x y) wrt x")
   (check-equal? (deriv '(- x y) 'x) 1 "deriv of (- x y) wrt x")
   (check-equal? (deriv '(sin x) 'x) '(cos x) "deriv of (sin x) wrt x")
   (check-equal? (deriv '(cos x) 'x) '(- 0 (sin x)) "deriv of (cos x) wrt x")
   (check-equal? (deriv '(sin(sin x)) 'x) '(* (cos (sin x)) (cos x)) "deriv of (sin (sin x)) wrt x")
   (check-equal? (deriv '(cos (cos x)) 'x) '(- 0 (* (sin (cos x)) (- 0 (sin x)))) "deriv of (cos (cos x)) wrt x")
   (check-equal? (deriv '(+ (sin x) 8) 'x) '(cos x) "deriv of (+ (sin x) 8) wrt x")
   (check-equal? (deriv '(+ (cos x) 8) 'x) '(- 0 (sin x)) "deriv of (+ (cos x) 8) wrt x")
   (check-equal? (deriv '(* (sin 0) x) 'x) 0 "deriv of (* (sin 0) x) wrt x")
   (check-equal? (deriv '(* (cos 0) x) 'x) 1 "deriv of (* (cos 0) x) wrt x")
   (check-equal? (deriv '(expt x 2) 'x) '(* 2 x) "deriv of (expt x 2) wrt x")
   (check-equal? (deriv '(expt x 4) 'x) '(* 4 (expt x 3)) "deriv of (expt x 2) wrt x")
   (check-equal? (deriv '(expt x 2) 'x) '(* 2 x) "deriv of (expt x 2) wrt x")
   (check-equal? (deriv '(* (expt x 1) x) 'x) '(+ x x) "deriv of (* (cos (expt x 1)) x) wrt x")
   (check-equal? (deriv '(* (cos (expt 0 1)) x) 'x) 1 "deriv of (* (cos (expt 0 1)) x) wrt x")
   (check-equal? (deriv '(expt x (sin x)) 'x) '(* (sin x) (expt x (- (sin x) 1))) "deriv of (deriv '(expt x (sin x)) 'x) wrt x")
   ;; simplification is not as clever as it could be in the following case:
   (check-equal? (deriv '(* (+ x 1) (+ x -1)) 'x) '(+ (+ x 1) (+ x -1)) "deriv of (* (+ x 1) (+ x -1)) wrt x")
   (check-equal? (deriv '(* (* x y) (+ x 3)) 'x) '(+ (* x y) (* y (+ x 3))) "complex deriv")
   ))

(require rackunit/text-ui)
;; this line runs the tests ....
(run-tests deriv-tests)
