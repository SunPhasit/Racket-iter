#lang racket
(define (AM . args)
  (/ (apply + args)
     (length args)))

(define (HM . args)
  (/ (length args)
     (apply + (map / args))))

(define (GM . args)
  (expt (apply * args) (/ (length args))))

(define (compare . args)
  (and (< (apply HM args) (apply GM args)) (< (apply GM args) (apply AM args))))

(define (am x y)
  (/ (+ x y) 2))

(define (hm x y)           ; From H.M. = 2/(1/x)+(1/y)
  (/ 2 (+ (/ x) (/ y))))

(define (hm2 x y)          ; H.M. = xy/((x+y)/2)
  (/ (* x y) (am x y)))

(define (hm3 x y)          ; H.M. = 1/((1/x)+(1/y)/2)
  (/ (am (/ x) (/ y))))

(define (pairwise-am l1 l2)
  (map am l1 l2))

(define (pairwise-hm l1 l2)
  (map hm l1 l2))