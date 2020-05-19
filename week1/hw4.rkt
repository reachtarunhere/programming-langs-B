#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

(define (sequence low high stride)
  (if (> low high)
      '()
      (cons low (sequence (+ low stride) high stride))))


(define (string-append-map xs suffix)
  (map (lambda (s) (string-append s suffix)) xs))


(define (list-nth-mod xs n)
  (cond [(< n 0) (error "list-nth-mod: negative number")]
        [(null? xs) (error "list-nth-mod: empty list")]
        [#t (car (list-tail xs (remainder n (length xs))))]))


(define (stream-for-n-steps s n)
  (if (eq? n 0) '()
         (let* ([evaled (s)]
                [current (car evaled)]
                [new-s (cdr evaled)])
           (cons current (stream-for-n-steps new-s (- n 1))))))


(define funny-number-stream
  (letrec ([five-wrapper (lambda (x) (if (eq? (remainder x 5) 0) (* -1 x) x))]
           [f (lambda (x) (cons (five-wrapper x) (lambda () (f (+ x 1)))))])
    (lambda () (f 1))))


(define dan-then-dog
  (letrec ([flipper (lambda (img) (if (eq? img "dan.jpg") "dog.jpg" "dan.jpg"))]
           [f (lambda (x) (cons x (lambda () (f (flipper x)))))])
    (lambda () (f "dan.jpg"))))


(define (stream-add-zero s)
  (let* ([one-realized (s)]
         [current (car one-realized)]
         [new-stream (cdr one-realized)])
    (lambda () (cons (cons 0 current) (lambda () ((stream-add-zero new-stream)))))))

    