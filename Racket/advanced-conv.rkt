#lang racket

(require racket/engine)
(provide main)

(define left '("Hello" "Fine thank you" "Splendid" "I'm doing well"
               "I'm fine, thanks" "Fine, just fine" "Simply dashing"))
(define right '("how are you?" "and you?" "how about you?" "and yourself?"
                "what about you?" "and how about you?" "how are you doing?"
                "how goes it with you?"))
(define (teacher-says ls rs)
  (displayln (string-append "THAI-TEACHER: " ls ", " rs)) (sleep 1))
(define (student-says ls rs)
  (displayln (string-append "THAI-STUDENT: " ls ", " rs)) (sleep 1))
(define (pick lst) (list-ref lst (random (length lst))))

(define conversation
  (engine
   (lambda (_)
     (begin
       (teacher-says (car left) (car right))
       (student-says (cadr left) (cadr right))
       (let loop ()
         (teacher-says (pick (cdr left))
                       (pick (cdr right)))
         (student-says (pick (cdr left))
                       (pick (cdr right)))
         (loop))))))

(define (main . args)
  (engine-run 20000 conversation))
                             