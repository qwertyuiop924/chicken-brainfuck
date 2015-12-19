;; This is the parser. The main function it exposes is `parse`, which takes
;; a list of tokens and builds an AST with the format:
;;
;; (DECREMENT)                            -> (((type . DECREMENT)))
;; (INCREMENT DECREMENT)                  -> (((type . INCREMENT)) ((type . DECREMENT)))
;; (BRACKET_OPEN INCREMENT BRACKET_CLOSE) -> (((type . WHILE) (((type . INCREMENT)) ((type . BACK)))) ((type . DECREMENT)))
;;
;; Each node is represented by a list. The first item of that list is always a
;; pair, representing the type. The rest of the elements is up to each parser.
;; So far, the only parser which adds more than a single pair to the list is
;; `parse-while`. For that parser, the second element is simply a list of nodes
;; to be executed.
;;
;; The fancy term for the design is LALR(1) recursive-descent parser. It
;; basically uses a single look-ahead of the token list, and depending on the
;; lookahead, calls a certain parser. That parser consumes the tokens and builds
;; the ast. This process is repeated until the tokens list is empty or an error
;; ocurred.
;;
(declare (unit parser))
(use srfi-1 anaphora);assoc and acond goodness
;; Utility function. Removes the first `n` elements from `lst`.
(define (remove-first lst n)
  (cond ((null? lst) 
         lst)
        ((equal? 0 n)
         lst)
        (else 
          (remove-first (cdr lst) (- n 1)))))

;; Parsers --------------------------------------------------------------------
(define (parse-increment tokens ast)
  (parse-statement (cdr tokens) (cons '((type . INCREMENT)) ast)))

(define (parse-cleanup tokens ast)
  (parse-statement (cdr tokens) (cons '((type . CLEANUP)) ast)))

(define (parse-escape tokens ast)
  (parse-statement (cdr tokens) (cons '((type . ESCAPE)) ast)))

(define (parse-decrement tokens ast)
  (parse-statement (cdr tokens) (cons '((type . DECREMENT)) ast)))

(define (parse-advance tokens ast)
  (parse-statement (cdr tokens) (cons '((type . ADVANCE)) ast)))

(define (parse-back tokens ast)
  (parse-statement (cdr tokens) (cons '((type . BACK)) ast)))

(define (parse-output tokens ast)
  (parse-statement (cdr tokens) (cons '((type . OUTPUT)) ast)))

(define (parse-input tokens ast)
  (parse-statement (cdr tokens) (cons '((type . INPUT)) ast)))

(define (parse-while tokens ast)
  (let* ((inside-tokens    (match-while-body (cdr tokens) '()))
         (statements       (parse-statement inside-tokens '()))
         (node             (list '(type . WHILE) statements))
         (remaining-tokens (remove-first (cdr tokens) (+ 1 (length inside-tokens)))))
    (parse-statement remaining-tokens (cons node ast))))

;; Matches the body of a WHILE, which is all tokens until a ]
(define (match-while-body tokens statements)
  (cond 
    ((or (null? tokens) (equal? 'BRACKET_CLOSE (car tokens))) 
     (reverse statements))
    (else
      (match-while-body (cdr tokens) (cons (car tokens) statements)))))

(define PARSER-MAP ;a map of tokens to parse functions
  `((PLUS         . ,parse-increment)
    (MINUS        . ,parse-decrement)
    (GREATER_THAN . ,parse-advance)
    (SMALLER_THAN . ,parse-back)
    (DOT          . ,parse-output)
    (COMMA        . ,parse-input)
    (BRACKET_OPEN . ,parse-while)
    (PERCENT      . ,parse-cleanup)
    (EOF          . ,parse-escape)
    (BANG         . ,parse-escape)))

;; Initial parser. Given some tokens, and an AST (which starts as an empty
;; list), call parsers using a single look-ahead and apply that parser to the
;; tokens. The parser might call other parsers before returning the ast.
(define (parse-statement tokens ast)
  (if (null? tokens) 
    (reverse ast)
    (let ((lookahead (car tokens)))
      (acond ((assoc lookahead PARSER-MAP)
              ((cdr it) tokens ast))
            (else 
              (print "Not a statement. Invalid token: " lookahead))))))
; ----------------------------------------------------------------------------

; Prettier version of `parse-statement`
(define (parse tokens)
  (parse-statement tokens '()))
