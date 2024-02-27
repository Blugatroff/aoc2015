#!r6rs
(library (aoc src main)
  (export main)
  (import
    (rnrs base)
    (rnrs io simple)
    (rnrs io ports)
    (rnrs control)
    (rnrs mutable-pairs)
    (rnrs lists)
    (rnrs records syntactic)
    (rnrs arithmetic bitwise)
    (rnrs bytevectors)
    (rnrs programs)
    (srfi :113)
    (srfi :128)
    (hashing md5)
    (aoc src util))

(define-record-type day (fields part-one part-two))

(define (day1 lines)
  (let ((first-line (car lines)))
    (let ((chars (string->list first-line)))
    (make-day
      (lambda (fail)
          (-
            (length (filter (lambda (c) (eq? c #\()) chars))
            (length (filter (lambda (c) (eq? c #\))) chars))))
      (lambda (fail)
        (call/cc (lambda (return)
          (fold-left
            (lambda (level char index)
              (cond
                ((< level 0) (return index))
                ((eq? char #\() (+ level 1))
                ((eq? char #\)) (- level 1))
                (else (fail "unrecognized character in input"))))
            0
            chars
            (enumerate chars))
          "Never entered the basement!")))))))

(define (day2 lines)
  (letrec* ((pairs (lambda (lst)
                 (if (null? lst)
                   '()
                   (list-concat (list
                                  (map (curry cons (car lst)) (cdr lst))
                                  (pairs (cdr lst)))))))
            (apply-pair (lambda (f p) (f (car p) (cdr p))))
            (mul-pair (curry apply-pair *))
            (add-pair (curry apply-pair +)))
    (make-day
      (lambda (fail)
        (sum (map
               (lambda (line)
                 (let ((sides (map mul-pair (pairs (map string->number (string-split (curry eq? #\x) line))))))
                   (+ (apply min sides) (* 2 (apply + sides)))))
               lines)))
      (lambda (fail)
        (sum (map
               (lambda (line)
                 (let* ((dimensions (map string->number (string-split (curry eq? #\x) line)))
                        (wraps (map add-pair (pairs dimensions))))
                  (+ (product dimensions) (* 2 (apply min wraps)))))
               lines))))))

(define-record-type v2 (fields x y))

(define (v2-eq a b) (and (equal? (v2-x a) (v2-x b)) (equal? (v2-y a) (v2-y b))))
(define (v2-ord a b) (if (< (v2-x a) (v2-x b)) #t (< (v2-y a) (v2-y b))))
(define (v2-hash v) (combine-hash (default-hash (v2-x v)) (default-hash (v2-y v))))
(define v2-comparator (make-comparator v2? v2-eq v2-ord v2-hash))

(define (pair-to-list p)
  (list (car p) (cdr p)))

(define (day3 lines)
  (let* ((first-line (car lines))
         (start (make-v2 0 0))
         (path (lambda (fail chars)
                   (let loop ((chars chars) (steps (list start)) (pos start))
                     (cond
                       ((null? chars) steps)
                       ((eq? (car chars) #\>) (let ((p (make-v2 (+ (v2-x pos) 1) (v2-y pos)))) (loop (cdr chars) (cons p steps) p)))
                       ((eq? (car chars) #\<) (let ((p (make-v2 (- (v2-x pos) 1) (v2-y pos)))) (loop (cdr chars) (cons p steps) p)))
                       ((eq? (car chars) #\^) (let ((p (make-v2 (v2-x pos) (+ (v2-y pos) 1)))) (loop (cdr chars) (cons p steps) p)))
                       ((eq? (car chars) #\v) (let ((p (make-v2 (v2-x pos) (- (v2-y pos) 1)))) (loop (cdr chars) (cons p steps) p)))
                       (else (fail "unexpected character in input"))))))
         (alternate (lambda (lst)
                      (let loop ((lst lst) (a '()) (b '()))
                        (if (null? lst)
                          (cons (reverse a) (reverse b))
                          (loop (cdr lst) b (cons (car lst) a)))))))
    (make-day
      (lambda (fail)
        (length (count-dedup v2-comparator (path fail (string->list first-line)))))
      (lambda (fail)
        (length (count-dedup v2-comparator (list-concat (map (curry path fail) (pair-to-list (alternate (string->list first-line)))))))))))

(define (day4 lines)
  (letrec ((input (car lines))
        (part (lambda (zeroes)
                (lambda (fail)
                  (let loop ((i 0))
                    (let ((h (md5->string (md5 (string->utf8 (string-append input (number->string i)))))))
                      (if (equal? zeroes (substring h 0 (string-length zeroes)))
                        i
                        (loop (+ 1 i)))))))))
    (make-day (part "00000") (part "000000"))))

(define (run-day day path)
  (display "Part one: ")
  (display (call/cc (day-part-one (call-with-input-file path (compose day read-lines)))))
  (display "\nPart two: ")
  (display (call/cc (day-part-two (call-with-input-file path (compose day read-lines))))))

(define days (list (cons 1 day1) (cons 2 day2) (cons 3 day3) (cons 4 day4)))

(define (main args)
  (let ((args (cdr args)))
    (if (null? args) (begin (display "expected day") (exit 1)))
    (let ((day-name (string->number (car args))))
        (if (equal? day-name #f)
          (begin (display "failed to parse day, expected a number") (exit 1))
          (let ((day (assoc day-name days)))
            (if (equal? day #f)
              (begin (display "This day is not implemented") (exit 1))
              (run-day (cdr day) (string-append "./inputs/" (number->string day-name) ".txt"))))))))
)

