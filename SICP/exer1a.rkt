#lang racket

;;;=== exer1a.rkt === Status : finished
; Exercise 1.1
; Exercise 1.2
; Exercise 1.3
; Exercise 1.4
; Exercise 1.5
; Exercise 1.6
; Exercise 1.7
; Exercise 1.8
; Exercise 1.9
; Exercise 1.10
; Exercise 1.11
; Exercise 1.12
; Exercise 1.13

; Exercise 1.1

10
; 10

(+ 5 3 4)
; 12

(- 9 1)
; 8

(/ 6 2)
; 3

(+ (* 2 4) (- 4 6))
; 6

(define a 3)
; (returns nothing)

(define b (+ a 1))
; (returns nothing)

(+ a b (* a b))
; 19

(= a b)
; #f

(if (and (> b a) (< b (* a b)))
      b
      a)
; 4

(cond ((= a 4) 6)
        ((= b 4) (+ 6 7 a))
        (else 25))
; 16

(+ 2 (if (> b a) b a))
; 6

(* (cond ((> a b) a)
           ((< a b) b)
           (else -1))
     (+ a 1))
; 16

; Exercise 1.2

(/ (+ 5
      4
      (- 2
         (- 3
            (+ 6
               4/5))))
   (* 3
      (- 6
         2)
      (- 2
         7)))
;-37/150

; Exercise 1.3
(define (sosq a b)
  (+ (* a a) (* b b)))
; (returns nothing)

(define (sosq-of-max a b c)       
  (cond ((and (>= a c) (>= b c)) (sosq a b))
        ((and (>= a b) (>= c b)) (sosq a c))
        (else (sosq b c))))
; (returns nothing)

; -Function test-
(sosq-of-max 3 4 5)
;41                     expected to get: 41

(sosq-of-max 2 3 3)
;18                     expected to get: 18

; Exercise 1.4

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a
                    b))
; (returns nothing)
; -Obsevation-
; if b is positive (b > 0) will return a+b
; if b is not positive (b <= 0) will return a-b
; This function can be written as a+|b|

; Exercise 1.5

(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))
; (returns nothing)

;(test 0 (p))
; (infinite function loop)             expected to get: 0
; We can conclude that racket will evaluate '0' and 'p' at the same time
; When it evaluates 'p', It calls 'p' which will call 'p' continuosly

; Exercise 1.6
(define (mysqrt x)
  (sqrt-iter 1.0 x))

(define (new-if predicate then-c else-c)
  (cond (predicate then-c)
        (else else-c)))

(define (good-enough? guess x)
  (< (abs (- (* guess guess) x)) 0.001))

(define (improve guess x)
  (/ (+ guess (/ x guess)) 2))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
; -Function test-
;(I have to comment this for compiling purposes)

;(mysqrt 1)
; (infinite loop)

;(mysqrt 2)
; (infinite loop)

; If I run procedure:mysqrt, I will get an infinite loop
; I believe that it does this because Racket evaluates both expressions of new-if
; So, sqrt -> sqrt-iter -> new-if -> good-enough?
;                                \-> sqrt-iter -> new-if ->...
;                                                       \->...
; The new-if loops infinitely

; == Page 33 ==
; Exercise 1.7
; -Problem with the first code-
; Using 0.001 as the amount of change might be large for some number; like 0.00001
; This can cause a large error while finding square root of small number
; While giving too much precision, causing more time consumption to
; compute square root of large numbers
; -Strategies-
; To check the change in number relative to original instead of a constant

(define (new-test guess x)
  (<= (abs (- (improve guess x) guess)) (* guess 0.001)))

(define (new-sqrtier guess x)
  (if (new-test guess x)
      guess
      (new-sqrtier (improve guess x) x)))

(define (new-sqrt x)
  (new-sqrtier 1 x))

; -Function test-

(new-sqrt 2)
;577/408

(new-sqrt 1)
;1

; Exercise 1.8

(define (cube-root x)
  (cube-rooteir 1 x))

(define (cube-tester guess x)
  (<= (abs (- (* guess guess guess) x)) (abs (* guess 0.001))))

(define (cube-improve guess x)
  (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))

(define (cube-rooteir guess x)
  (if (cube-tester guess x)
      guess
      (cube-rooteir (cube-improve guess x) x)))

; Exercise 1.9
; -Model I-
;(define (+ a b)
;  (if (= a 0) b (inc (+ (dec a) b))))

;(+ 4 5)                                                                 ;4 != 0
;(inc (+ (dec 4) 5))                                                     ;(dec 4) = 3 -> 3 != 0
;(inc (inc (+ (dec (dec 4)) 5)))                                         ;(dec (dec 4)) = 2 -> 2 != 0
;(inc (inc (inc (+ (dec (dec (dec 4))) 5))))                             ;(dec (dec (dec 4))) = 1 -> 1 != 0
;(inc (inc (inc (inc (+ (dec (dec (dec (dec 4)))) 5)))))                 ;(dec (dec (dec (dec 4)))) = 0 -> 0 = 0
;(inc (inc (inc (inc (+ (dec (dec (dec 3))) 5)))))
;(inc (inc (inc (inc (+ (dec (dec 2)) 5))))))
;(inc (inc (inc (inc (+ (dec 1) 5))))))
;(inc (inc (inc (inc (+ 0 5))))))
;(inc (inc (inc (inc 5)))))
;(inc (inc (inc 6))))
;(inc (inc 7)))
;(inc 8))
;9
; Recursive function

; -Model II-
;(define (+ a b)
;  (if (= a 0) b (+ (dec a) (inc b))))

;(+ 4 5)
;(+ (dec 4) (inc 5))                                                   ;(dec 4) = 3 != 0
;(+ (dec (dec 4)) (inc (inc 5)))                                       ;(dec (dec 4)) = 2 != 0
;(+ (dec (dec (dec 4))) (inc (inc (inc 5))))                           ;(dec (dec (dec 4))) = 1 != 0
;(+ (dec (dec (dec (dec 4)))) (inc (inc (inc (inc 5)))))               ;(dec (dec (dec (dec 4)))) = 0 = 0
;(+ (dec (dec (dec 3))) (inc (inc (inc 6))))
;(+ (dec (dec 2)) (inc (inc 7)))
;(+ (dec 1) (inc 8))
;(+ 0 9)
;9
; Recursive function

; Exercise 1.10

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

(A 1 10)
;1024

(A 2 4)
;65536

(A 3 3)
;65536

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

;_________________________________________
;|   n   |   f   |   g   |   h   |   k   |
;|_______|_______|_______|_______|_______|
;|   1   |   2   |   2   |   2   |   5   |
;|   2   |   4   |   4   |   4   |  20   |
;|   3   |   6   |   8   |  16   |  45   |
;|   4   |   8   |  16   | 65536 |  80   |
;|_______|_______|_______|_______|_______|
;| Functions - write in terms of n       |
;|_______________________________________|
;|       |       |       |    n  |       |
;|       |       |   n   |   2   |    2  |
;|   n   |  2n   |  2    |  2    |  5n   |
;|_______|_______|_______|_______|_______|

; Exercise 1.11

(define (f2 n)
  (let loop ((a 0) (b 1) (c 2) (N n))
    (if (= N 0)
        a
        (loop b c (+ c (* 2 b) (* 3 a)) (- N 1)))))

; Exercise 1.12

(define (Pa row ele)
  (cond ((or (= ele 1)
             (= ele row)) 1)
        (else (+ (Pa (- row 1) ele)
                 (Pa (- row 1) (- ele 1))))))

; Exercise 1.13
; For this, I will use induction
; Let p = (1 + r(5))/2, q = (1 - r(5))/2 and F(n) to be the fibbonanci function where n >= 0
; Let statement Q(n) be that F(n) = the closest integer to (p^n - q^n)/r(5)
; We'll need to prove the base cases, F(0) and F(1)
;  From F(n) = (p^n - q^n)/r(5)
;       F(0) = (p^0 - q^0)/r(5)
;            = (1-1)/r(5) = 0 #
;       F(1) = (p^1 - q^1)/r(5)
;            = ((1 + r(5))/2 - (1 - r(5))/2)/r(5) 
;            = (r(5)/2 + r(5)/2)/r(5) = r(5)/r(5) = 1 #
; So, Q(0) and Q(1) is true
; Now, we'll assume that Q(n-1) and Q(n) is true
; From here, we'll prove that Q(n+1) is true
; F(n+1) = F(n-1) + F(n)
;        = ((p^(n-1) - q^(n-1))/r(5)) + ((p^n - q^n)/r(5))
;        = (p^(n-1)*(1+p)-(q^(n-1)*(1+q)))/r(5)
;        = (p^(n-1)*(p^2)-(q^(n-1)*(q^2)))/r(5)
; F(n+1) = (p^(n+1)-q^(n+1))/r(5) => Q(n+1) is true #
; Therefore, Q(n) is true for all n >= 0
