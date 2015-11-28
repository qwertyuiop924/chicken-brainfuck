(declare (uses tokenizer))
(declare (uses parser))
(use extras) ; read-line

; This is our memory memory, a collection of buckets, each bucket has numbers.
;(define data (vector-unfold (lambda (n) 0) 100))
;(define current-position   0) ; Start at bucket 0
;(define current-loop-index 0) ; [ was opened in a bucket, store bucket number
;
;(define (advance)
;  (set! current-position (+ 1 (current-position))))
;
;(define (current-value)
;  (vector-ref data current-position))

(define (eval-brainfuck input)
  (parse (tokenize input)))

(define (read-brainfuck)
  (begin
    (format #t "brainfuck> ")
    (read-line)))

(define (repl)
  (begin
    (print (eval-brainfuck (read-brainfuck)))
    (repl)))

(repl)
