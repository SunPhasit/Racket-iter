#lang racket
(module+ test
  (require rackunit))

; 1) Write a function that counts the amount of odd numbers in the list
(define (oddcount list)
  (odd-iter list 0))
(define (odd-iter list count)
  (if (empty? list)
      count
      (if (odd? (first list))
          (odd-iter (rest list) (+ 1 count))
          (odd-iter (rest list) count))))

(module+ test
  (check-equal? (oddcount '(1 2 3 4 5 6 7 8 9 0)) 5))

; 2) Write a function that finds the sum of the odd numbers in the list
(define (oddsum list)
  (odsum-iter list 0))
(define (odsum-iter list sum)
  (if (empty? list)
      sum
      (if (odd? (first list))
          (odsum-iter (rest list) (+ (first list) sum))
          (odsum-iter (rest list) sum))))

(module+ test
  (check-equal? (oddsum '(1 2 3 4 5 6 7 8 9 0)) (+ 1 3 5 7 9)))

; 2') Write a function that finds the product of the odd numbers in the list
(define (oddproduct list)
  (odpro-iter list 1))
(define (odpro-iter list prod)
  (if (empty? list)
      prod
      (if (odd? (first list))
          (odpro-iter (rest list) (* (first list) prod))
          (odpro-iter (rest list) prod))))

(module+ test
  (check-equal? (oddproduct '(1 2 3 4 5 6 7 8 9 0)) (* 1 3 5 7 9)))

; 3) Write a function that finds the factorial of a number
(define (facto n)
  (fact-iter 1 n 1))
(define (fact-iter a n st)
  (cond ((> a n) st)
        (else (fact-iter (+ a 1) n (* a st)))))

(module+ test
  (check-equal? (facto 10) (* 1 2 3 4 5 6 7 8 9 10)))

; 4) Write a function that finds the bifactorial of a number
(define (bifacto n)
  (bif-iter 0 n 1))
(define (bif-iter st n re)
  (cond ((= st 0)
         (if (odd? n)
             (bif-iter 1 n re)
             (bif-iter 2 n re)))
        ((> st n) re)
        (else (bif-iter (+ st 2) n (* re st)))))

(module+ test
  (check-equal? (bifacto 8) (* 8 6 4 2))
  (check-equal? (bifacto 5) (* 5 3 1)))

;; The other, shorter way :

(define (bifacto2 n)
  (foldl *
         1
         (filter (if (odd? n)
                     odd?
                     even?)
                 (map add1 (range n)))))

(module+ test
  (check-equal? (bifacto2 8) (* 8 6 4 2))
  (check-equal? (bifacto2 5) (* 5 3 1)))
      
  