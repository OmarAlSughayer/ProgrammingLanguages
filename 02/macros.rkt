#lang racket

;Omar Adel AlSughayer (1337255)
;CSE 341 AB
;Assignment 2 - Questions 3 & 4

(require rackunit)
(require rackunit/text-ui)

;Answer for Question 3
;a structure to hold a delayed expression.
;is-evaluated is true if the expression has been evaluated already,
;and false if not.  value is either a lambda (if is-evaluated is
;false) or the resulting value (if is-evaluated is true). 
(struct delay-holder (is-evaluated value) #:transparent #:mutable)

;a macro version of Racket's delay function
(define-syntax my-delay
  (syntax-rules ()
    [(my-delay e1) (delay-holder #f (lambda () e1))]
    [(my-delay e1 e2 ...)
     (cons (delay-holder #f (lambda () e1)) (my-delay e2 ...))]
  )
)

;force an expression to be evaluated, if it's not already
(define (my-force held)
  (cond [(null? held)]
        [(not (pair? held)) (forceOne held)]
        [else (forceOne (car held)) (my-force (cdr held))]
   )
)

;this is the old my-force that was defind in class, unchanged.
;since every single expression is wraped in a seperate lambda a caller function (the new my-foce)
;was needed to evaluate every expression individually by using this function as a helper
(define (forceOne holder)
  (cond [(delay-holder-is-evaluated holder) (delay-holder-value holder)]
        [else (set-delay-holder-is-evaluated! holder #t)
              (set-delay-holder-value! holder ((delay-holder-value holder)))
              (delay-holder-value holder)]
  )
)

;Unit tests for the my-delay and my-force
(define my-delay-force-tests
  (test-suite
   "tests for the my-and macro"
 
   ;tests that my-delay/my force return identical result to the built-in delay/force functions 
   (check-equal? (my-force (my-delay (+ 5 7)))
                 (force (delay (+ 5 7)))
                 "checks that my-delay/my-force matches delay/force")
   
   (check-equal? (my-force (my-delay (and "why doesn't work?" #f) (list? " why?!!") (+ 3 4) '() (+ 2 3)))
                 (force (delay (and "why doesn't work?" #f) (list? " why?!!") (+ 3 4) '() (+ 2 3)))
                 "checks that my-delay/my-force matches delay/force")
   (check-equal? (my-force (my-delay '()))
                 (force (delay '()))
                 "checks that my-delay/my-force matches delay/force")

   ;tests that the expressions are not evaluated when the delay is constructed
   (check-equal? (let* ([x 10] [d (my-delay (set! x 2))]) x)
                 10
                 "checks that the (set! x 2) expression will not be evaluated until force")
   (check-equal? (let* ([x 10] [d (set! x 2)]) x)
                 2
                 "just a sanity check, to make sure that my-delay is what halts (set! x 2) from excution")
  
   ;tests that the expression is not re-evaluated when called again
   (check-equal? (let* ([x 10] [d (my-delay (set! x (- x 1)))]) (my-force  d) (my-force  d) x)
                 9
                 "checks that forcing d twice will evaluate (set! x (- x 1)) only once, hence x = 10-1 = 9")
   
   )
)

;this line runs the tests for the poly-multiply function and all of its helper functinons
(run-tests my-delay-force-tests)


;Answer for Question 4
;a macro version of Racket's and function
(define-syntax my-and
  (syntax-rules ()
    [(my-and) #t]
    [(my-and e1) e1]
    [(my-and e1 e2 ...) (if e1 (my-and e2 ...) #f)]
  )
)

;Unit tests for the my-and macro
(define my-and-tests
  (test-suite
   "tests for the my-and macro"
 
   ;tests for the my-and macro
   (check-equal? (my-and)
                 #t
                 "checks that an expression with no arguments evaluates to true")
   (check-equal? (my-and #t)
                 #t
                 "checks that #t evaluates to #t")
   (check-equal? (my-and #f)
                 #f
                 "checks that #f evaluates to #f")
   (check-equal? (my-and #t #t #t #t)
                 #t
                 "checks that (and #t #t #t #t) evaluates to #t")
   (check-equal? (my-and #t #t #f #t)
                 #f
                 "checks that (and #t #t #f #t) evaluates to #f")
   (check-equal? (my-and 1 2)
                 2
                 "checks that (and 1 2) evaluates to 2")
   (check-equal? (my-and 1 #f 2)
                 #f
                 "checks that (and 1 #f 2) evaluates to #f")
   (check-equal? (my-and 2 #t)
                 #t
                 "checks that (and 2 #t) evaluates to #t")
   )
)

;this line runs the tests for the poly-multiply function and all of its helper functinons
(run-tests my-and-tests)