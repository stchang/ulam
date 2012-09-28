#lang racket
(require plot)

(require (rename-in (only-in racket map) [map eager-map]))

;; calculate prime (sicp style)
(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))


;; some stream fns
(define (from n)
  (cons n (lazy (from (add1 n)))))

(define (take n lst)
  (if (zero? n)
      null
      (cons (car (force lst)) (take (sub1 n) (cdr (force lst))))))

(define (filter p? lst)
  (let ([flst (force lst)])
    (if (null? flst)
        null
        (let ([x (car flst)])
          (if (p? x)
              (cons x (lazy (filter p? (cdr flst))))
              (filter p? (cdr flst)))))))
      
(define (map f . lsts)
  (let ([flsts (eager-map force lsts)])
  (if (null? (car flsts))
      null
      (cons (apply f (eager-map car flsts)) (lazy (apply map (cons f (eager-map cdr flsts))))))))


;; calculate ulams
(define id (λ (x) x))

(define (ulams-from x y x-op y-op)
  (cons (vector x y)
        (lazy
         (let ([x-new (x-op x)]
               [y-new (y-op y)])
           (cond
             ;; SE, turn N
             [(and (> x-new 0) (= (sub1 x-new) (* -1 y-new)))
              (ulams-from x-new y-new id add1)]
             ;; NE, turn W
             [(and (> x-new 0) (= x-new y-new))
              (ulams-from x-new y-new sub1 id)]
             ;; NW, turn S
             [(and (< x-new 0) (= (* -1 x-new) y-new))
              (ulams-from x-new y-new id sub1)]
             ;; SW, turn E
             [(and (< x-new 0) (= x-new y-new))
              (ulams-from x-new y-new add1 id)]
             [else (ulams-from x-new y-new x-op y-op)])))))


;; define streams

(define nats (from 1))

(define ulams (ulams-from 0 0 add1 id))

(define prime-ulams 
  (map cdr
       (filter (match-lambda [(cons n _) (prime? n)])
               (map cons nats ulams))))
  
(define pts (take 10000 prime-ulams))

(plot (points pts #:size 1))