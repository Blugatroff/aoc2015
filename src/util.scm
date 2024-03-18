#!r6rs
(library (aoc src util)
  (export
    read-lines
    dbg
    list-span
    list-split
    list-concat
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
    trace-show
    apply-pair
    lifted-or
    lifted-and
    ->
    ->>
    flip
    match
    match-lambda
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

(define (trace-show name v)
  (display name) (display ": ") (display v) (newline) v)

(define (apply-pair f p)
  (f (car p) (cdr p)))

(define-syntax lifted-or
  (syntax-rules (helper)
    ((_ helper x r) (or . r))
    ((_ helper x r f rest ...)
     (lifted-or helper x ((f x) . r) rest ...))
    ((lifted-or fs ...)
     (lambda (x) (lifted-or helper x () fs ...)))))

(define-syntax lifted-and
  (syntax-rules (helper)
    ((_ helper x r) (and . r))
    ((_ helper x r f rest ...)
     (lifted-and helper x ((f x) . r) rest ...))
    ((lifted-and fs ...)
     (lambda (x) (lifted-and helper x () fs ...)))))

(define-syntax reverse-order
  (syntax-rules ()
    ((_ e) (reverse-order e ()))
    ((_ (e . rest) r) (reverse-order rest (e . r)))
    ((_ () r) r)))

(define-syntax ->
  (syntax-rules ()
    ((_ v)
     v)
    ((_ v (f ...))
     (f ... v))
    ((_ v f)
     (f v))
    ((_ v (f ...) fs ...)
     (-> (f ... v) fs ...))
    ((_ v f fs ...)
     (-> (f v) fs ...))))

(define-syntax ->>
  (syntax-rules ()
    ((_ (f ...))
     (lambda (x) (f ... x)))
    ((_ f)
     f)
    ((_ (f ...) fs ...)
     (lambda (x) (-> x (f ...) fs ...)))
    ((_ f fs ...)
     (lambda (x) (-> (f x) fs ...)))))

(define-syntax flip
  (syntax-rules ()
    ((_ f)
     (lambda (a b) (f b a)))
    ((_ f a b)
     (f b a))))

(define-syntax match-lambda
  (syntax-rules ()
    ((_ (args ...) patterns ...)
     (lambda (args ... x)
       (match x patterns ...)))))

(define-syntax match
  (syntax-rules ()
    ((_ arg (case pat selection))
     (let ((pat arg)) selection))
    ((_ arg (case pat selection) next rest ...)
     (let ((pat arg)) (match selection next rest ...)))
    ((_ arg (case pat default selection))
     (apply (case-lambda [pat selection] [x default]) arg))
    ((_ arg (case pat default selection) next rest ...)
     (apply (case-lambda [pat (match selection next rest ...)] [x default]) arg))
    ))

)
