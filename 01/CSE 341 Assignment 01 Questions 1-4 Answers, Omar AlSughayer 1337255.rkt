#lang racket

;Omar Adel AlSughayer (1337255)
;CSE 341 AB
;Assignment 1 - Questions 1-4

(require rackunit)
(require rackunit/text-ui)

;Answer for Question 1
;Takes a list of numbers and returns a list of their squares
;or the empty list if the passed list was empty
(define (squares nums)
  (if (null? nums) nums
      (append (list (* (car nums) (car nums))) (squares (cdr nums)))))

;Unit tests for the squares function
(define squares-tests
  (test-suite
   "tests for the squares function"
   (check-equal? (squares (list 2 3 4)) '(4 9 16) "the squares of an integers list")
   (check-equal? (squares (list 0.5 1/2 1/3)) '(0.25 1/4 1/9) "the squares of an doubles list")
   (check-equal? (squares (list )) '() "the empty list")
   (check-equal? (squares (list 4)) '(16) "a list with only one item")
   ))

;this line runs the tests for the squares function
(run-tests squares-tests)


;Answer for Question 2
;Takes a list of numbers and returns a list of their squares
;or the empty list if the passed list was empty using the built-in map function
(define (map-squares nums)
  (map (lambda (x) (* x x)) nums))

;Unit tests for the map-squares function
(define map-squares-tests
  (test-suite
   "tests for the map-squares function"
   (check-equal? (map-squares (list 2 3 4)) '(4 9 16) "the squares of an integers list")
   (check-equal? (map-squares (list 0.5 1/2 1/3)) '(0.25 1/4 1/9) "the squares of an doubles list")
   (check-equal? (map-squares (list )) '() "the empty list")
   (check-equal? (map-squares (list 4)) '(16) "a list with only one item")
   ))

;this line runs the tests for the map-squares function
(run-tests map-squares-tests)

;Answer for Question 3
;Returns #t if the passed list was in strictly ascending order, and #f otherwise
;The empty list and any list with one item are defind to be strictly ascending
;This function is NOT tail-recursive because it alters the result of the recursive call back
;by applying it to the AND function together with the a value withing this frame in the stack.
(define (ascending nums)
  (if (or (null? nums) (eq? (length nums) 1)) #t
      (and (< (car nums) (cadr nums)) (ascending (cdr nums)))))

;Unit tests for ascending function
(define ascending-tests
  (test-suite
   "tests for the ascending function"
   (check-equal? (ascending (list 2 3 4)) #t "a strictly ascending list")
   (check-equal? (ascending (list 1 2 4 3)) #f "a not strictly ascending list")
   (check-equal? (ascending (list 4 3 2 1 0 -1)) #f "inverse list")
   (check-equal? (ascending (list 1 2 2 3)) #f "a list with equal numbers")
   (check-equal? (ascending (list )) #t "the empty list")
   (check-equal? (ascending (list 4)) #t "a list with only one item")
   ))

;this line runs the tests for the ascending function
(run-tests ascending-tests)

;Answer for Question 4
;Takes a list represinting a let* expression and returns an equavilant expresion using let.
(define (let*->let exp)
  (if (eq? (car exp) 'let*)
      (innerVar (cadr exp) (cddr exp))
      exp)
  )
  
;Helper function for let*->let, recursively transforms every sub level of the let* expression to
;a let experssion
(define (innerVar var rest)
  (cond [(> (length var) 1) (list 'let (list (car var)) (innerVar (cdr var) rest))]
        [(equal? (length var) 1) (append (list 'let (list (car var))) rest)]
        [else (append (list 'let var) rest)]
  ))

;Unit tests for let*->let function
(define let*->let-tests
  (test-suite
   "tests for the let*->let function"
   (check-equal? (let*->let
                  '(let* ([x 3] [y (+ x 1)] [z (+ x y)]) (+ x y z)))
                 '(let ([x 3]) (let ([y (+ x 1)]) (let ([z (+ x y)]) (+ x y z))))
                 "a let* statment with 3 variables and no side effect statments")
   (check-equal? (let*->let
                 '(let* ([x 3] [y 4]) (+ x y)))
                 '(let ((x 3)) (let ((y 4)) (+ x y)))
                 "a let* statment with 2 variables and no side effect statments")
   (check-equal? (let*->let
                 '(let* () (+ x 1)))
                 '(let () (+ x 1))
                 "a let* statment with no variables and no side effect statments")
   (check-equal? (let*->let
                 '(let* ([x 5] [y (+ x 1)]) (printf "x=~a\n" x) (printf "y=~a\n" y) (+ x y)))
                 '(let ((x 5)) (let ((y (+ x 1))) (printf "x=~a\n" x) (printf "y=~a\n" y) (+ x y)))
                 "a let* statment with 2 variables and 2 side effect statments")
   (check-equal? (eval (let*->let '(let* ([x 3] [y (+ x 1)]) (+ x y))) (make-base-namespace))
                 7
                 "checking syntax and evaluated values")
   ))

;this line runs the tests for the ascending function
(run-tests let*->let-tests)

