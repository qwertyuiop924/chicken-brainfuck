;; BRAINFUCK INTERPRETER
;; This is a simple brainfuck interpreter. For information on the language
;; see: https://en.wikipedia.org/wiki/Brainfuck
;;
;; It's implemented using a typical `tokenizer -> parser -> interpreter` chain.
;;
;; Author: Federico Ramirez <fedra.arg@gmail.com>
;; URL: https://github.com/gosukiwi/chicken-brainfuck
;;
(declare (uses tokenizer))
(declare (uses parser))
(require-extension vector-lib) ; vectors
(use extras) ; read-line

(define DATA-SIZE 100) ; This is our memory, a collection of buckets, each bucket has numbers.
(define data (make-vector DATA-SIZE 0))
(define current-position 0) ; Start at bucket 0

(define (current-value)
  (vector-ref data current-position))

(define (current-value-set! value)
  (vector-set! data current-position value))

;; Evaluators
;; ----------------------------------------------------------------------------
(define (advance node)
  (set! current-position (modulo (+ 1 current-position) DATA-SIZE)))

(define (escape node)
  (exit 0))

(define (cleanup node)
  (vector-map! (lambda (i x) 0) data))

(define (back node)
  (set! current-position (modulo (- current-position 1) DATA-SIZE)))

(define (decrement node)
  (current-value-set! (- (current-value) 1)))

(define (increment node)
  (current-value-set! (+ 1 (current-value))))

(define (output node)
  (format #t "~A" (integer->char (current-value))))

(define (input node)
  (current-value-set! (char->integer (read-char))))

(define (while node)
  (let ((bucket-position current-position)
        (statements (cadr node)))
    (unless (equal? 0 (vector-ref data bucket-position))
        (eval-ast statements)
        (while node))))
;; ----------------------------------------------------------------------------

(define (eval-node node)
  (let* ((type (cdar node))
         (evaluators `((DECREMENT . ,decrement)
                       (INCREMENT . ,increment)
                       (ADVANCE   . ,advance)
                       (BACK      . ,back)
                       (OUTPUT    . ,output)
                       (INPUT     . ,input)
                       (WHILE     . ,while)
                       (CLEANUP   . ,cleanup)
                       (ESCAPE    . ,escape)))
         (fn (cdr (assoc type evaluators))))
    (apply fn (list node))))

(define (eval-ast ast)
  (unless (null? ast)
      (eval-node (car ast))
      (eval-ast  (cdr ast))))

(define (eval-brainfuck input)
  (eval-ast (parse (tokenize input))))

(define (read-brainfuck)
    (format #t "brainfuck> ")
    (read-line))

(define (repl)
    (eval-brainfuck (read-brainfuck))
    (repl))

(begin 
  (print "Welcome to CHICKEN Brainfuck!")
  (repl))
