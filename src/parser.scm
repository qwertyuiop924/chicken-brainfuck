(declare (unit parser))
(require-extension vector-lib) ; vectors
(require-extension srfi-13)    ; string goodies

(define (remove-first lst n)
  (cond ((null? lst) 
         lst)
        ((equal? 0 n)
         lst)
        (else 
          (remove-first (cdr lst) (- n 1)))))

(define (parse-increment tokens ast)
  (parse-tokens (cdr tokens) (cons '(type . INCREMENT) ast)))

(define (parse-decrement tokens ast)
  (parse-tokens (cdr tokens) (cons '(type . DECREMENT) ast)))

(define (parse-input tokens ast)
  (parse-tokens (cdr tokens) (cons '(type . INPUT) ast)))

(define (parse-loop tokens ast)
  (let* ((inside-tokens    (parse-loop-statements (cdr tokens) '()))
         (statements       (parse-tokens inside-tokens '()))
         (node             (list '(type . WHILE) statements))
         (remaining-tokens (remove-first (cdr tokens) (+ 1 (length inside-tokens)))))
    (print inside-tokens)
    (print remaining-tokens)
    (parse-tokens remaining-tokens (cons node ast))))

(define (parse-loop-statements tokens statements)
  (cond 
    ((or (null? tokens) (equal? 'BRACKET_CLOSE (car tokens))) 
     (reverse statements))
    (else
      (parse-loop-statements (cdr tokens) (cons (car tokens) statements)))))

; Given some tokens, and an AST (which starts as a default list), find
; a parser using a single look-ahead.
; Returns a list of matched nodes. Some output examples:
; (DECREMENT)                            -> ((type . DECREMENT))
; (DECREMENT INCREMENT)                  -> ((type . INCREMENT) (type . DECREMENT))
; (BRACKET_OPEN INCREMENT BRACKET_CLOSE) -> ((type . WHILE) (body . ((type . INCREMENT))))
(define (parse-tokens tokens ast)
  (if (null? tokens) 
    (reverse ast)
    (let ((lookahead (car tokens)))
      (cond ((equal? 'PLUS lookahead)
             (parse-increment tokens ast))
            ((equal? 'MINUS lookahead)
             (parse-decrement tokens ast))
            ((equal? 'BRACKET_OPEN lookahead)
             (parse-loop tokens ast))
            (else 
              (print "Invalid token:" lookahead))))))

(define (parse tokens)
  (parse-tokens tokens '()))
