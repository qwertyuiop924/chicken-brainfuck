; BRAINFUCK INTERPRETER
; This is a simple brainfuck interpreter. For information on the language
; see: https://en.wikipedia.org/wiki/Brainfuck
;
; It's implemented using a typical `tokenizer -> parser -> interpreter` chain.
;
; Author: Federico Ramirez <fedra.arg@gmail.com>
; URL: https://github.com/gosukiwi/chicken-brainfuck
(declare (uses tokenizer))
(declare (uses parser))
(require-extension vector-lib) ; vectors
(use extras) ; read-line

; This is our memory memory, a collection of buckets, each bucket has numbers.
(define data (vector-unfold (lambda (n) 0) 100))
; Start at bucket 0
(define current-position 0) 

(define (current-value)
  (vector-ref data current-position))

(define (current-value-set! value)
  (vector-set! data current-position value))

; Evaluators
; ----------------------------------------------------------------------------
(define (advance node)
  (set! current-position (+ 1 current-position)))

(define (back node)
  (set! current-position (- current-position 1)))

(define (decrement node)
  (current-value-set! (- (current-value) 1)))

(define (increment node)
  (current-value-set! (+ 1 (current-value))))

(define (output node)
  (format #t "~A" (integer->char (current-value))))

(define (input node)
  (current-value-set! (read-line)))

(define (while node)
  (let ((bucket-position current-position)
        (statements (cadr node)))
    (if (not (equal? 0 (vector-ref data bucket-position)))
      (begin
        (eval-ast statements)
        (while node)))))
; ----------------------------------------------------------------------------

(define (eval-node node)
  (let* ((type (cdar node))
        (evaluators '(
                      (DECREMENT . decrement)
                      (INCREMENT . increment)
                      (ADVANCE   . advance)
                      (BACK      . back)
                      (OUTPUT    . output)
                      (INPUT     . input)
                      (WHILE     . while)
                      ))
        (fn-name (cdr (assoc type evaluators))))
    (apply (eval fn-name) (list node))))

(define (eval-ast ast)
  (if (not (null? ast))
    (begin
      (eval-node (car ast))
      (eval-ast  (cdr ast)))))

(define (eval-brainfuck input)
  (eval-ast (parse (tokenize input))))

(define (read-brainfuck)
  (begin
    (format #t "brainfuck> ")
    (read-line)))

(define (repl)
  (begin
    (eval-brainfuck (read-brainfuck))
    (repl)))

(repl)
