#lang racket

(define (gen-list n)
  (if (= n 1)
      (list (random 2))
      (cons (random 2) (gen-list (- n 1)))))

(define (gen-grid n m)
  (if (= m 1)
      (list(gen-list n))
      (cons (gen-list n) (gen-grid n (- m 1)))))

(define (gen-list-border list)
  (cons 0 (append list '(0))))

(define (n-zeroes n)
  (if (= n 0)
      null
      (cons 0 (n-zeroes (- n 1)))))

(define (gen-grid-border grid)
  (append (list (n-zeroes (+ 2 (length (car grid)))))
          (map gen-list-border grid)
          (list (n-zeroes (+ 2 (length (car grid)))))))

(define (nth n list)
  (if (= n 1)
      (car list)
      (nth (- n 1) (cdr list))))

(define (grid-get x y grid)
  (nth x (nth y grid)))

(define (all-zeroes? list)
  (= 0 (apply + list)))

(define (trim-last list)
  (if (null? (cdr list))
      null
      (cons (car list) (trim-last (cdr list)))))

(define (trim list)
  (cdr (trim-last list)))

(define (neighbours x y grid)
  (list (grid-get (- x 1) (- y 1) grid)
        (grid-get (- x 1) (+ y 0) grid)
        (grid-get (- x 1) (+ y 1) grid)
        (grid-get (+ x 0) (- y 1) grid)
        (grid-get (+ x 0) (+ y 1) grid)
        (grid-get (+ x 1) (- y 1) grid)
        (grid-get (+ x 1) (+ y 0) grid)
        (grid-get (+ x 1) (+ y 1) grid)))

(define (what-you-become x y grid)
  (if (or (= (map + neighbours(x y grid)) 3) (and (= (map + neighbours(x y grid)) 2) (= (grid-get x y grid) 1)))
      1
      0))

(define (move x p list)
  (if (= x 1)
      (cons p (cdr list))
      (cons (car list) (move (- x 1) p (cdr list)))))

(define (all-true list)
  (if (null? list)
      #t
      (if (car list)
          (all-true (cdr list))
          #f)))

;(define (disp list)
;  (if (null? list)
;      null


;(define (disp-cell n)
;  (if (= n 1)
;      (hc-append

;New list from a grid of m lists of n bools
(define (new-list n m grid)
  (if (= n 1)
      
      
      
      
      

(define (next grid)
  (begin (display (trim (map trim grid)))
         (if (all-true (map all-zeroes? grid))
             grid
             (next (move grid)))))

;(define (game-of-life n m)
;  (next (gen-grid-border (gen-grid n m))))
  


