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
    (rnrs eval)
    (only (srfi :1) any count take iota)
    (srfi :113)
    (srfi :128)
    (only (srfi :152) string-split string-take-while string-drop-while string-trim-both string-contains string-filter string-null?)
    (hashing md5)
    (aoc src util))

(define-record-type day (fields part-one part-two))

(define (day1 lines fail)
  (let ((first-line (car lines)))
    (let ((chars (string->list first-line)))
    (make-day
      (lambda (fail)
          (-
            (count (->> (eq? #\()) chars)
            (count (->> (eq? #\))) chars)))
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

(define (day2 lines fail)
  (letrec* ((pairs (lambda (lst)
                 (if (null? lst)
                   '()
                   (list-concat (list
                                  (map (->> (cons (car lst))) (cdr lst))
                                  (pairs (cdr lst)))))))
            (mul-pair (->> (apply-pair *)))
            (add-pair (->> (apply-pair +))))
    (make-day
      (lambda (fail)
        (-> lines
              (map
               (lambda (line)
                 (let ((sides (-> (string-split line "x")
                                    (map string->number)
                                    pairs
                                    (map mul-pair))))
                   (-> sides (apply +) (* 2) (+ (apply min sides))))))
              sum))
      (lambda (fail)
        (sum (map
               (lambda (line)
                 (let* ((dimensions (map string->number (string-split line "x"))))
                  (-> dimensions
                        pairs
                        (map add-pair)
                        (apply min)
                        (* 2)
                        (+ (product dimensions)))))
               lines))))))

(define-record-type v2 (fields x y))

(define (v2-eq a b) (and (equal? (v2-x a) (v2-x b)) (equal? (v2-y a) (v2-y b))))
(define (v2-ord a b) (if (< (v2-x a) (v2-x b)) #t (< (v2-y a) (v2-y b))))
(define (v2-hash v) (combine-hash (default-hash (v2-x v)) (default-hash (v2-y v))))
(define v2-comparator (make-comparator v2? v2-eq v2-ord v2-hash))

(define (pair-to-list p)
  (list (car p) (cdr p)))

(define (day3 lines fail)
  (let* ((first-line (match lines (case (first-line . rest) (fail "Expected a line of input but got none.") first-line)))
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
        (length (count-dedup v2-comparator (list-concat (map (->> (path fail)) (pair-to-list (alternate (string->list first-line)))))))))))

(define (day4 lines fail)
  (letrec ((input (match lines (case (first-line . rest) (fail "Expected a line of input but got none.") first-line)))
        (part (lambda (zeroes)
                (lambda (_)
                  (let loop ((i 0))
                    (let ((h (md5->string (md5 (string->utf8 (string-append input (number->string i)))))))
                      (if (-> zeroes string-length (substring h 0) (equal? zeroes))
                        i
                        (loop (+ 1 i)))))))))
    (make-day (part "00000") (part "000000"))))

(define (day5 lines _)
  (make-day
    (lambda (fail)
      (letrec ((vowels (string->list "aeiou"))
               (forbidden (list "ab" "cd" "pq" "xy"))
               (n-vowels (->> string->list (count (->> (flip member vowels)))))
               (has-double? (lambda (str) (cond
                                            [(< (string-length str) 2) #f]
                                            [(equal? (string-ref str 0) (string-ref str 1)) #t]
                                            [else (has-double? (substring str 1 (string-length str)))])))
               (nothing-forbidden? (lambda (str)
                                     (not (any (->> (string-find str)) forbidden))))
               (is-nice? (lambda (str) (and (>= (n-vowels str) 3) (has-double? str) (nothing-forbidden? str)))))
        (count is-nice? lines)))
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
               (contains-repeating-pair (->>
                                          pairs
                                          remove-overlapping
                                          (count-dedup (make-default-comparator))
                                          (any (->> cdr (< 1)))))
               (is-nice? (lambda (str) (let ((chars (string->list str)))
                                         (and (has-spaced-repeat chars) (contains-repeating-pair chars))))))
        (-> lines (filter is-nice?) length)))))

(define-record-type rect (fields x1 y1 x2 y2))
(define-record-type point (fields x y))
(define (rect-contains r p)
  (and (>= (point-x p) (rect-x1 r)) (<= (point-x p) (rect-x2 r))
       (>= (point-y p) (rect-y1 r)) (<= (point-y p) (rect-y2 r))))

(define (day6 lines fail)
  (letrec* ((parse-line-instr-kind (lambda (line)
                                         (cond
                                           [(string-starts-with "toggle" line) 'toggle]
                                           [(string-starts-with "turn on" line) 'turn-on]
                                           [(string-starts-with "turn off" line) 'turn-off])))
            (parse-number (lambda (s) (let ((n (string->number s)))
                                        (if (eq? n #f) (fail (string-append "failed to parse " s)) n))))
                (parse-line-rect (lambda (line)
                                   (let* ((r (string-drop-while line (->> char-numeric? not)))
                                          (x1 (parse-number (string-take-while r char-numeric?)))
                                          (r (string-drop-while r char-numeric?))
                                          (r (string-drop-while r (->> char-numeric? not)))
                                          (y1 (parse-number (string-take-while r char-numeric?)))
                                          (r (string-drop-while (string-drop-while r char-numeric?) (->> char-numeric? not)))
                                          (x2 (parse-number (string-take-while r char-numeric?)))
                                          (r (string-drop-while (string-drop-while r char-numeric?) (->> char-numeric? not)))
                                          (y2 (parse-number r)))
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
                                             [else (loop (->> state not) (cdr instrs))]))))
                (determine-point-brightness (lambda (p)
                                              (fold-left (lambda (brightness instr)
                                                           (cond
                                                             [(not (rect-contains (cdr instr) p)) brightness]
                                                             [(eq? 'turn-on (car instr)) (+ brightness 1)]
                                                             [(eq? 'turn-off (car instr)) (max 0 (- brightness 1))]
                                                             [else (+ brightness 2)])) 0 instrs)))
                (all-points (apply append (map (lambda (x) (map (lambda (y) (make-point x y)) (iota 1000)))
                                               (iota 1000)))))
    (make-day (lambda (_) (count determine-point-state all-points))
              (lambda (_) (fold-left (lambda (sum point) (+ sum (determine-point-brightness point))) 0 all-points)))))

(define (day7 lines fail)
  (letrec* ((parse-failure (lambda () (fail "Failed to parse")))
           (parse (match-lambda (already-defined)
                    (case (line . rest) '() (-> (string-split line "->") (map string-trim-both)))
                    (case (op dst) (parse-failure) (string-filter (lifted-or (->> char-alphabetic? not) char-lower-case?) op))
                    (case lowercase (-> (string-split lowercase " ") (map string-trim-both) (filter (->> string-null? not))))
                    (case (x . segments) (parse-failure)
                      (let* ((sanitize (lambda (x) (if (string->number x) (string->number x) (string->symbol (string-append "__" x)))))
                             (sx (sanitize x))
                             (y (if (null? segments) "0" (car segments)))
                             (sy (sanitize y))
                             (is-defined (lambda (x) (if (string->number x) #t (member x already-defined)))))
                        (if (and (is-defined x) (is-defined y))
                          (cons
                            (quasiquote ((unquote (sanitize dst))
                                         (bitwise-and #xFFFF
                                                      (unquote (cond
                                                                 [(string-contains op "AND") (quasiquote (bitwise-and (unquote sx) (unquote sy)))]
                                                                 [(string-contains op "OR") (quasiquote (bitwise-ior (unquote sx) (unquote sy)))]
                                                                 [(string-contains op "LSHIFT") (quasiquote (bitwise-arithmetic-shift-left (unquote sx) (unquote sy)))]
                                                                 [(string-contains op "RSHIFT") (quasiquote (bitwise-arithmetic-shift-right (unquote sx) (unquote sy)))]
                                                                 [(string-contains op "NOT") (quasiquote (bitwise-not (unquote sx)))]
                                                                 [else (sanitize op)])))))
                            (parse (cons dst already-defined) rest))
                          (parse already-defined (append rest (list line))))))))
           (result-wire (if (< 100 (length lines)) '__a '__i))
           (eval-with-env (lambda (form) (eval form (environment '(rnrs base) '(rnrs arithmetic bitwise)))))
           (eval-wires (lambda (lines) (eval-with-env (quasiquote (let* (unquote (parse '() lines)) (unquote result-wire))))))
           (b (eval-wires lines)))
    (make-day
      (lambda (_) b)
      (lambda (_)
        (eval-wires (cons
                      (string-append (number->string b) " -> b")
                      (filter
                        (->> (flip string-split "->") cadr string-trim-both (equal? "b") not)
                        lines)))))))

(define (run-day prepare-day file)
  (let* ((lines (read-lines file)))
    (call/cc (lambda (done)
               (let ((day (prepare-day lines (lambda (message)
                                               (display "Error: ") (display message) (newline)
                                               (done)))))
                 (display "Part one: ")
                 (display (call/cc (day-part-one day)))
                 (display "\nPart two: ")
                 (display (call/cc (day-part-two day))))))))

(define days (list
               (cons 1 day1)
               (cons 2 day2)
               (cons 3 day3)
               (cons 4 day4)
               (cons 5 day5)
               (cons 6 day6)
               (cons 7 day7)))

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

