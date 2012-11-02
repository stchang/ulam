#lang racket

;; calculate prime (sicp style)
(define (prime? n) (= n (smallest-divisor n)))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (* test-divisor test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))


;; some stream fns
(define (from n)
  (stream-cons n (from (add1 n))))

(define (stream-take n lst)
  (if (zero? n)
      null
      (cons (stream-first lst) (stream-take (sub1 n) (stream-rest lst)))))


(define (stream-maps f . lsts)
  (if (stream-empty? (stream-first lsts))
      null
      (stream-cons (apply f (map stream-first lsts))
                   (apply stream-maps (cons f (map stream-rest lsts))))))


;; calculate ulams
(define id (λ (x) x))

(define (ulams-from x y x-op y-op)
  (stream-cons (vector x y)
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
                   [else (ulams-from x-new y-new x-op y-op)]))))


;; define streams

(define nats (from 1))

(define ulams (ulams-from 0 0 add1 id))

(define prime-ulams 
  (stream-map cdr
              (stream-filter (match-lambda [(cons n _) (prime? n)])
                             (stream-maps cons nats ulams))))
  
(define pts (stream-take 10000 prime-ulams))


(require plot)
(plot (points pts #:size 1))