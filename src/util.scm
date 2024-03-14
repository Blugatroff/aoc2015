#!r6rs
(library (aoc src util)
  (export
    read-lines
    dbg
    list-span
    list-split
    list-concat
    compose
    curry
    enumerate
    sum
    product
    count-dedup
    combine-hash
    string-find
    string-find-char
    string-find-str
    pairs
    string-starts-with
  )
  (import
    (rnrs) (rnrs mutable-pairs)
    (srfi :113))

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

(define (string-find s char-or-string)
  (if (char? char-or-string)
    (string-find-char s char-or-string)
    (string-find-str s char-or-string)))

(define (string-find-char s c)
  (let ((n (string-length s)))
    (let loop ((cursor 0))
      (cond
        [(eq? cursor n) #f]
        [(eq? c (string-ref s cursor)) cursor]
        [else (loop (+ 1 cursor))]))))

(define (string-find-str s target)
  (let ((n (string-length s)))
    (let loop ((cursor 0))
      (if (= cursor n)
        #f
        (let ((i (string-find-char (substring s cursor n) (string-ref target 0))))
          (cond
            [(eq? i #f) (loop (+ 1 cursor))]
            [(> (+ (string-length target) cursor) n) #f]
            [(equal? target (substring s cursor (+ (string-length target) cursor))) cursor]
            [else (loop (+ 1 cursor))]))))))

(define (count-dedup comparator lst)
  (bag->alist (list->bag comparator lst)))

(define (combine-hash a b) (+ (* 31 a) b))

(define (pairs l)
  (cond
    [(null? l) '()]
    [(null? (cdr l)) '()]
    [else (cons (list (car l) (cadr l)) (pairs (cdr l)))]))

(define (string-starts-with prefix s)
  (if (< (string-length s) (string-length prefix))
    #f
    (equal? prefix (substring s 0 (string-length prefix)))))

)
