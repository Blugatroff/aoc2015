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
    (rnrs unicode)
    (only (srfi :1) any count take iota)
    (srfi :113)
    (srfi :128)
    (only (srfi :152) string-split string-take-while string-drop-while)
    (hashing md5)
    (aoc src util)
    (only (scheme) trace-let trace-lambda))

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
                 (let ((sides (map mul-pair (pairs (map string->number (string-split line "x"))))))
                   (+ (apply min sides) (* 2 (apply + sides)))))
               lines)))
      (lambda (fail)
        (sum (map
               (lambda (line)
                 (let* ((dimensions (map string->number (string-split line "x")))
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

(define (foo a) (display a) (newline) a)

(define (day5 lines)
  (make-day
    (lambda (fail)
      (letrec ((vowels (string->list "aeiou"))
               (forbidden (list "ab" "cd" "pq" "xy"))
               (n-vowels (lambda (str) (count (lambda (c) (member c vowels)) (string->list str))))
               (has-double? (lambda (str) (cond
                                            [(< (string-length str) 2) #f]
                                            [(equal? (string-ref str 0) (string-ref str 1)) #t]
                                            [else (has-double? (substring str 1 (string-length str)))])))
               (nothing-forbidden? (lambda (str)
                                     (not (any (lambda (forbidden) (string-find str forbidden)) forbidden))))
               (is-nice? (lambda (str) (and (>= (n-vowels str) 3) (has-double? str) (nothing-forbidden? str)))))
        (count (lambda (str) (and (is-nice? str))) lines)))
    (lambda (fail)
      (letrec ((remove-overlapping (lambda (pairs) (cond
                                                     [(null? pairs) '()]
                                                     [(null? (cdr pairs)) pairs]
                                                     [(and (equal? (caar pairs) (cadar pairs)) (equal? (car pairs) (cadr pairs))) (cons (car pairs) (cddr pairs))]
                                                     [else (cons (car pairs) (remove-overlapping (cdr pairs)))])))
               (has-spaced-repeat (lambda (chars)
                                    (cond
                                      [(null? chars) #f]
                                      [(null? (cdr chars)) #f]
                                      [(null? (cddr chars)) #f]
                                      [(equal? (car chars) (caddr chars)) #t]
                                      [else (has-spaced-repeat (cdr chars))])))
               (contains-repeating-pair (lambda (chars) (any (lambda (entry) (< 1 (cdr entry)))
                                                             (count-dedup (make-default-comparator)
                                                                          (remove-overlapping (pairs chars))))))
               (is-nice? (lambda (str) (let ((chars (string->list str)))
                                         (and (has-spaced-repeat chars) (contains-repeating-pair chars))))))
        (length (filter is-nice? lines))))))

(define-record-type rect (fields x1 y1 x2 y2))
(define-record-type point (fields x y))
(define (rect-contains r p)
  (and (>= (point-x p) (rect-x1 r)) (<= (point-x p) (rect-x2 r))
       (>= (point-y p) (rect-y1 r)) (<= (point-y p) (rect-y2 r))))

(define (day6 lines)
  (letrec* ((parse-line-instr-kind (lambda (line)
                                         (cond
                                           [(string-starts-with "toggle" line) 'toggle]
                                           [(string-starts-with "turn on" line) 'turn-on]
                                           [(string-starts-with "turn off" line) 'turn-off])))
                (parse-line-rect (lambda (line)
                                   (letrec* ((r1 (string-drop-while line (compose not char-numeric?)))
                                             (x1 (string->number (string-take-while r1 char-numeric?)))
                                             (r2 (string-drop-while r1 char-numeric?))
                                             (r3 (string-drop-while r2 (compose not char-numeric?)))
                                             (y1 (string->number (string-take-while r3 char-numeric?)))
                                             (r4 (string-drop-while (string-drop-while r3 char-numeric?) (compose not char-numeric?)))
                                             (x2 (string->number (string-take-while r4 char-numeric?)))
                                             (r5 (string-drop-while (string-drop-while r4 char-numeric?) (compose not char-numeric?)))
                                             (y2 (string->number r5)))
                                     (make-rect x1 y1 x2 y2))))
                (parse-line (lambda (line)
                              (cons (parse-line-instr-kind line) (parse-line-rect line))))
                (instrs (map parse-line lines))
                (rev-instrs (reverse instrs))
                (determine-point-state (lambda (p)
                                         (let loop ((state (lambda (a) a)) (instrs rev-instrs))
                                           (cond
                                             [(null? instrs) (state #f)]
                                             [(not (rect-contains (cdar instrs) p)) (loop state (cdr instrs))]
                                             [(eq? 'turn-on (caar instrs)) (state #t)]
                                             [(eq? 'turn-off (caar instrs)) (state #f)]
                                             [else (loop (compose not state) (cdr instrs))]))))
                (determine-point-brightness (lambda (p)
                                              (fold-left (lambda (brightness instr)
                                                           (cond
                                                             [(not (rect-contains (cdr instr) p)) brightness]
                                                             [(eq? 'turn-on (car instr)) (+ brightness 1)]
                                                             [(eq? 'turn-off (car instr)) (max 0 (- brightness 1))]
                                                             [else (+ brightness 2)])) 0 instrs)))
                (all-points (apply append (map (lambda (x) (map (lambda (y) (make-point x y)) (iota 1000)))
                                               (iota 1000)))))
    (make-day (lambda (fail) (count determine-point-state all-points))
              (lambda (fail) (fold-left (lambda (sum point) (+ sum (determine-point-brightness point))) 0 all-points)))))

(define (run-day mkDay file)
  (let* ((lines (read-lines file))
         (day (mkDay lines)))
    (display "Part one: ")
    (display (call/cc (day-part-one day)))
    (display "\nPart two: ")
    (display (call/cc (day-part-two day)))))

(define days (list (cons 1 day1) (cons 2 day2) (cons 3 day3) (cons 4 day4) (cons 5 day5) (cons 6 day6)))

(define (main args)
  (let* ((args (cdr args))
         (stdin (member "--stdin" args)))
    (if (null? args) (begin (display "expected day") (exit 1)))
    (let ((day-name (string->number (car args))))
        (if (equal? day-name #f)
          (begin (display "failed to parse day, expected a number") (exit 1))
          (let ((day (assoc day-name days)))
            (if (equal? day #f)
              (begin (display "This day is not implemented") (exit 1))
              (if (member "--stdin" args)
                (run-day (cdr day) (current-input-port))
                (call-with-input-file (string-append "./inputs/" (number->string day-name) ".txt") (lambda (file)
                                                                                                   (run-day (cdr day) file))))))))))
)

