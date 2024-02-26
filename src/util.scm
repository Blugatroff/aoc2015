#!r6rs
(library (aoc src util)
  (export read-lines dbg list-span list-split list-concat compose curry enumerate sum product string-split count-dedup)
  (import (rnrs) (rnrs mutable-pairs))

(define (read-lines port)
  (let ((line (get-line port)))
    (if (eof-object? line)
      '()
      (cons line (read-lines port)))))

(define (dbg name a)
  (begin (display name) (display ": ") (display a) (newline) a))

(define (list-span f lst)
  (if (null? lst)
    (list (list) lst)
    (if (f (car lst))
      (let ((next (list-span f (cdr lst))))
        (list (cons (car lst) (car next)) (cadr next)))
      (list (list) lst))))

(define (list-split del lst)
  (cond ((null? lst) lst)
  (else 
    (let ((span (list-span (lambda (v) (not (eq? del v))) lst)))
    (let ((head (car span)) (tail (cadr span)))
    (cond 
      ((and (null? head) (null? tail)) (list))
      ((null? head) (cons (list) (list-split del (cdr tail))))
      ((null? tail) (list head))
      (else (cons head (list-split del (cdr tail))))))))))

(define (list-concat lsts)
  (if (null? lsts) lsts 
    (if (null? (car lsts))
      (list-concat (cdr lsts))
      (cons (car (car lsts)) (list-concat (cons (cdr (car lsts)) (cdr lsts)))))))

(define (compose . funs) 
  (if (null? funs)
    (lambda (x) x)
    (let
      ((next (apply compose (cdr funs))))
      (lambda (x) ((car funs) (next x))))))

(define (curry f x) (lambda (y) (f x y)))

(define (enumerate lst)
  (let loop ((i 0) (lst lst))
    (if (null? lst)
      '()
      (cons i (loop (+ 1 i) (cdr lst))))))

(define (sum lst) (fold-left + 0 lst))
(define (product lst) (fold-left * 1 lst))

(define (string-split sep s)
  (let loop ((s s) (cursor 0))
    (let ((n (string-length s)))
      (cond
        ((eq? n 0) '())
        ((and (eq? cursor 0) (sep (string-ref s 0))) (loop (substring s 1 n) 0))
        ((= cursor n) (list (substring s 0 cursor)))
        ((sep (string-ref s cursor)) (cons (substring s 0 cursor) (loop (substring s (+ 1 cursor) n) 0)))
        (else (loop s (+ 1 cursor)))))))

(define (count-dedup lst)
  (let loop ((lst lst) (counts '()))
    (cond
      ((null? lst) counts)
      ((assoc (car lst) counts)
       (let ((slot (assoc (car lst) counts)))
         (begin
           (set-cdr! slot (+ 1 (cdr slot)))
           (loop (cdr lst) counts))))
       (else (loop (cdr lst) (cons (cons (car lst) 1) counts)))))))

