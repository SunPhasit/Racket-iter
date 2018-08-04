#lang racket
(define c (cons 3 4))                        ; Not a list, last element is not empty
(define d (cons 3 (cons 4 (cons 5 6))))      ; Not a list, last element is not empty
(define e (cons 1 (cons 2 (cons 3 null))))   ; Is a list, last element is empty - shorter alt : (list 1 2 3)
(length (list 5 10 11 12))                   ; procedure:length counts number of elements
(apply * (list 6 7 8))                       ; procedure:apply operates procedure to elements; returns a number
(map / (list 1 1/2 1/3 1/4))                 ; procedure:map operates procedure to each element; returns a list
(map + (list 1 2 3 4)                  
       (list 3 4 5 6))
(reverse (list 1 2 3 4 5 6))                 ; procedure:reverse reverses elements
(car (list 1 2 3 4))                         ; procedure:car returns first element
(cdr (list 1 2 3 4))                         ; procedure:cdr returns the list without the first element
