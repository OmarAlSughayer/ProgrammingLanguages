#lang racket

;Omar Adel AlSughayer (1337255)
;CSE 341 AB
;Assignment 2 - Questions 1 & 2

(require rackunit)
(require rackunit/text-ui)

;Answer for Question 1
;takes two polynomials in symbolic variables form for and returns the result as a list arranged by power
(define (poly-multiply p1 p2) (normalize (sort (multiply p1 p2))))

;takes two polynomials in symbolic variables form and returns an unarranged result
(define (multiply p1 p2)
  (cond [(or (null? p1) (null? p2)) null];0 * f(x) = 0
        [(and (number? (car p1)) (number? (car p2)))
              (list (* (car p1) (car p2)) (+ (cadr p1) (cadr p2)))];multiplying two cells
        [(number? (car p1)) (cons (multiply p1 (car p2)) (multiply p1 (cdr p2)))];multiplying a cell by a list
        [else (append (multiply (car p1) p2) (multiply (cdr p1) p2))];multiplying a list by a list
  )
)

;takes a polynomial in symbolic variables form and returns a sorted version by descending power
(define (sort poly)
  (cond [(null? poly) poly]
        [else (insert-into-place (car poly) (sort (cdr poly)))]
  )
)

;inserts one element in its proper place inside a list arranged in descending order
(define (insert-into-place e p)
  (cond [(null? p) (cons e p)];if the list is empty
        [(> (cadr e) (cadar p)) (cons e p)];if e had a higher power than the all elements in p
        [else (cons (car p) (insert-into-place e (cdr p)))];
  )
)

;takes a descendingly-sorted polynomial in symbolic variables form and normalizes it
(define (normalize poly)
  (cond [(or (null? poly) (null? (cdr poly))) poly];if poly is null or has one item -> normalized
        ;if the power of the first two elements match
        [(eq? (cadar poly) (cadadr poly))
              (remove-zero
               (normalize (cons (list (+ (caar poly) (caadr poly)) (cadar poly)) (cddr poly))))]
        ;if the power of the first two elements does not match
        [else (remove-zero (cons (car poly) (normalize (cdr poly))))]
  )
)

;removes any zero terms from a polynomial in symbolic variables form
(define (remove-zero poly) (filter (lambda (t) (not (eq? 0 (car t)))) poly))

;Unit tests for the poly-multiply function and all of its helper functinons
(define poly-multiply-tests
  (test-suite
   "tests for the poly-multiply function and all of its helper functinons"
   
   ;tests for the poly-multiply function
   (check-equal? (poly-multiply '((-3 4) (1 1) (5 0)) '())
                 '()
                 "check that:  (-3x^4 + x + 5) * 0 = 0")
   (check-equal? (poly-multiply '((1 3) (1 2) (1 1) (1 0)) '((1 1) (-1 0)))
                 '((1 4) (-1 0))
                 "check that: (x^3 + x^2 + x + 1) * (x - 1) = x^4 - 1")
   (check-equal? (poly-multiply '() '((1 2)))
                 '()
                 "check that: 0 * x^2 = 0")
   (check-equal? (poly-multiply '((1 3) (1 1) (-1 0)) '((-5 0)))
                 '((-5 3) (-5 1) (5 0))
                 "check that: (x^3 + x - 1) * -5 = -5x^3 -5x + 5")
   (check-equal? (poly-multiply '((-10 2) (100 1) (5 0)) '((1 999) (-1 7) (1 1) (3 0)))
                 '((-10 1001) (100 1000) (5 999) (10 9) (-100 8) (-5 7) (-10 3) (70 2) (305 1) (15 0))
                 "check that: (-10x^2 + 100x + 5) * (x^999 − x^7 + x + 3)
                  = -10x^1001 + 100x^1000 + 5x^999 + 10x^9 - 100x^8 - 5x^7 - 10x^3 + 70x^2 + 305x + 15")
   (check-equal? (poly-multiply '((3 0)) '((1 1)))
                 '((3 1))
                 "check that: 3 * x = 3x")
   (check-equal? (poly-multiply '((1 1)) '((3 0)))
                 '((3 1))
                 "check that: x * 3 = 3x")

   ;tests for the multiply function
   (check-equal? (multiply '((-3 4) (1 1) (5 0)) '())
                 '()
                 "check that:  (-3x^4 + x + 5) * 0 = 0")
   (check-equal? (multiply '((1 3) (1 2) (1 1) (1 0)) '((1 1) (-1 0)))
                 '((1 4) (-1 3) (1 3) (-1 2) (1 2) (-1 1) (1 1) (-1 0))
                 "check that: (x^3 + x^2 + x + 1) * (x - 1) = x^4 - x^3 + x^3 -x^2 + x^2 - x + x - 1")
   (check-equal? (multiply '() '((1 2)))
                 '()
                 "check that: 0 * x^2 = 0")
   (check-equal? (multiply '((1 3) (1 1) (-1 0)) '((-5 0)))
                 '((-5 3) (-5 1) (5 0))
                 "check that: (x^3 + x - 1) * -5 = -5x^3 -5x + 5")
   (check-equal? (multiply '((-10 2) (100 1) (5 0)) '((1 999) (-1 7) (1 1) (3 0)))
                 '((-10 1001) (10 9) (-10 3) (-30 2) (100 1000) (-100 8) (100 2) (300 1) (5 999) (-5 7) (5 1) (15 0))
                 "check that: (-10x^2 + 100x + 5) * (x^999 - x^7 + x + 3)
                  = -10x^1001 + 10x^9 - 10x^3 - 30x^2 + 100x^1000 - 100x^8 + 100x^2 + 300x + 5x^999 - 5x^7 + 5x + 15")
   (check-equal? (multiply '((3 0)) '((1 1)))
                 '((3 1))
                 "check that: 3 * x = 3x")
   (check-equal? (multiply '((1 1)) '((3 0)))
                 '((3 1))
                 "check that: x * 3 = 3x")
   
   ;tests for the sort function
   (check-equal? (sort '((5 0) (-3 4) (1 1)))
                 '((-3 4) (1 1) (5 0))
                 "check that:  5 - 3x^4 + x is sorted into -3x^4 + x + 5")
   (check-equal? (sort '())
                 '()
                 "check that: 0 is sorted into 0")
   (check-equal? (sort '((5 0)))
                 '((5 0))
                 "check that: 5 is sorted into 5")
   (check-equal? (sort '((5 0) (-3 4) (1 1) (4 9)))
                 '((4 9) (-3 4) (1 1) (5 0))
                 "check that:  5 - 3x^4 + x + 4x^9 is sorted into 4x^9 - 3x^4 + x + 5")
   (check-equal? (sort '((-5 0) (-5 1) (3 2) (4 3) (-10 4) (4 5)))
                 '((4 5) (-10 4) (4 3) (3 2) (-5 1) (-5 0))
                 "check that: -5 - 5x + 3x^2 + 4x^3 - 10x^4 + 4x^5 is sorted into 4x^5 - 10x^4 + 4x^3 + 3x^2 - 5x - 5")

   ;tests for the normalize function
   (check-equal? (normalize '((-3 4) (1 1) (5 0)))
                 '((-3 4) (1 1) (5 0))
                 "check that: -3x^4 + x + 5 is normalized into -3x^4 + x + 5")
   (check-equal? (normalize '())
                 '()
                 "check that: 0 is normalized into 0")
   (check-equal? (normalize '((5 0)))
                 '((5 0))
                 "check that: 5 is normalized into 5")
   (check-equal? (normalize '((1 4) (-1 4)))
                 '()
                 "check that: x^4 - x^4 is sorted into 0")
   (check-equal? (normalize '((4 5) (-10 5) (4 3) (3 2) (-5 1) (5 1)))
                 '((-6 5) (4 3) (3 2))
                 "check that: 4x^5 - 10x^5 + 4x^3 + 3x^2 -5x + 5x is normalized -6x^5 + 4x^3 + 3x^2")
   (check-equal? (normalize '((1 4) (-1 4) (5 2)))
                 '((5 2))
                 "check that: x^4 - x^4 + 5x^2 is sorted into 5x^2")
   )
)

;this line runs the tests for the poly-multiply function and all of its helper functinons
(run-tests poly-multiply-tests)

;Answer for Question 2
;converts a polynomial in symbolic variables form into evaluable Racket code
(define (poly->code poly var) (polyToCode poly var #t (> (length poly) 1)))

;converts a polynomial in symbolic variables form into evaluable Racket code within a single '+ operation
(define (polyToCode poly var first? large)
  (cond [(null? poly) 0];the empty list = the zero polynomial
        ;the poly is only one term and the original poly has >1 term
        [(and large (eq? (length poly) 1)) (list (term->code (car poly) var))]
        ;the poly is only one term and the original poly has only one term
        [(eq? (length poly) 1) (term->code (car poly) var)]
        ;poly has >1 terms and this is the first term
        [first? (append (list '+ (term->code (car poly) var)) (polyToCode (cdr poly) var #f large))]
        ;poly has >1 terms and this is not the first term
        [else (append (list (term->code (car poly) var)) (polyToCode (cdr poly) var first? large))]
  )     
)

;takes one term of a polynomial in symbolic form and converts it into evaluatable Racket code
(define (term->code term var)
  (cond [(null? term) null]
        [(eq? (car term) 0) null];coefficient is zero -> return 0
        [(eq? (cadr term) 0) (car term)];exponent is zero -> return the coefficient
        [(and (eq? (car term) 1) (eq? (cadr term) 1)) var];exponent and coefficient is one -> return the variable
        [(eq? (car term) 1) (list 'expt var (cadr term))];coefficient is one -> return '(expt variable exponent)
        [(eq? (cadr term) 1) (list '* (car term) var)];exponent is one -> return '(* coefficient variable)
        [else (list '* (car term) (list 'expt var (cadr term)))];return '(* coefficient (expt variable exponent))
  )
)

;Unit tests for the poly->code function and all of its helper functinons
(define poly->code-tests
  (test-suite
   "tests for the poly->code function and all of its helper functinons"
 
   ;tests for the poly-> function
   (check-equal? (poly->code '((1 3) (5 2) (7 1) (10 0)) 'x)
                 '(+ (expt x 3) (* 5 (expt x 2)) (* 7 x) 10)
                 "check that: '((1 3) (5 2) (7 1) (10 0)) converts to '(+ (expt x 3) (* 5 (expt x 2)) (* 7 x) 10)")
   (check-equal? (poly->code '((1 1) (-10 0)) 'x)
                 '(+ x -10)
                 "check that: '((1 1) (-10 0) 'x) converts to '(+ x -10)")
   (check-equal? (poly->code '((1 1)) 'x)
                 'x
                 "check that: '((1 1) 'x) converts to 'x")
   (check-equal? (poly->code  '((10 0)) 'x)
                 10
                 "check that: '((10 0) 'x) converts to 10")
   (check-equal? (poly->code '((1 0)) 'x)
                 1
                 "check that: '((1 0) 'x) converts to 1")
   (check-equal? (poly->code '() 'x)
                 0
                 "check that: '() converts to 0")
   (check-equal? (poly->code '((10 1)) 'x)
                 '(* 10 x)
                 "check that: '(10 1) converts to '(* 10 x)")
     (check-equal? (eval (let ([p1 '((1 3) (1 2) (1 1) (1 0))]
               [p2 '((1 1) (-1 0))]
               [x 4]) (poly->code (poly-multiply p1 p2) x)) (make-base-namespace))
                 '255
                 "check that: (x^3 + x^2 + x + 1) * (x - 1) | (x = 4) = 255")
     (check-equal? (let ([p1 '((1 3) (1 2) (1 1) (1 0))]
               [p2 '((1 1) (-1 0))]
               [x 4]) (* (eval (poly->code p1 x) (make-base-namespace)) (eval (poly->code p2 x) (make-base-namespace))))
                 '255
                 "check that: [(x^3 + x^2 + x + 1) | (x = 4)] * [(x - 1) | (x = 4)] = 255")

     ;tests for the term->code function
     (check-equal? (term->code '(10 1) 'x)
                 '(* 10 x)
                 "check that: '(10 1) converts to '(* 10 x)")
     (check-equal? (term->code '(10 2) 'x)
                 '(* 10 (expt x 2))
                 "check that: '(10 2) converts to '(* 10 (expt x 2))")
     (check-equal? (term->code '(0 2) 'x)
                 '()
                 "check that: '(0 2) converts to '()")
     (check-equal? (term->code '(1 2) 'x)
                 '(expt x 2)
                 "check that: '(1 2) converts to '(expt x 2)")
     (check-equal? (term->code '(7 0) 'x)
                 7
                 "check that: '(7 0) converts to 7")
     (check-equal? (term->code '(1 1) 'x)
                 'x
                 "check that: '(1 1) converts to 'x")
   )
)

;this line runs the tests for the poly-multiply function and all of its helper functinons
(run-tests poly->code-tests)