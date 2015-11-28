; This is the parser. The main function it exposes is `parse`, which takes
; a list of tokens and builds an AST with the format:
;
; (DECREMENT)                            -> (((type . DECREMENT)))
; (DECREMENT INCREMENT)                  -> (((type . INCREMENT)) ((type . DECREMENT)))
; (BRACKET_OPEN INCREMENT BRACKET_CLOSE) -> (((type . WHILE) (((type . INCREMENT)) ((type . BACK)))) ((type . DECREMENT)))
;
; Each node is represented by a list. The first item of that list is always a
; pair, representing the type. The rest of the elements is up to each parser.
; So far, the only parser which adds more than a single pair to the list is
; `parse-while`. For that parser, the second element is simply a list of nodes
; to be executed.
;
; The fancy term for the design is LALR(1) recursive-descent parser. It
; basically uses a single look-ahead of the token list, and depending on the
; lookahead, calls a certain parser. That parser consumes the tokens and builds
; the ast. This process is repeated until the tokens list is empty or an error
; ocurred.
;
(declare (unit parser))

; Utility function. Removes the first `n` elements from `lst`.
(define (remove-first lst n)
  (cond ((null? lst) 
         lst)
        ((equal? 0 n)
         lst)
        (else 
          (remove-first (cdr lst) (- n 1)))))

; Parsers --------------------------------------------------------------------
(define (parse-increment tokens ast)
  (tokens->ast (cdr tokens) (cons '((type . INCREMENT)) ast)))

(define (parse-decrement tokens ast)
  (tokens->ast (cdr tokens) (cons '((type . DECREMENT)) ast)))

(define (parse-advance tokens ast)
  (tokens->ast (cdr tokens) (cons '((type . ADVANCE)) ast)))

(define (parse-back tokens ast)
  (tokens->ast (cdr tokens) (cons '((type . BACK)) ast)))

(define (parse-output tokens ast)
  (tokens->ast (cdr tokens) (cons '((type . OUTPUT)) ast)))

(define (parse-input tokens ast)
  (tokens->ast (cdr tokens) (cons '((type . INPUT)) ast)))

(define (parse-while tokens ast)
  (let* ((inside-tokens    (parse-while-statements (cdr tokens) '()))
         (statements       (tokens->ast inside-tokens '()))
         (node             (list '(type . WHILE) statements))
         (remaining-tokens (remove-first (cdr tokens) (+ 1 (length inside-tokens)))))
    (tokens->ast remaining-tokens (cons node ast))))

(define (parse-while-statements tokens statements)
  (cond 
    ((or (null? tokens) (equal? 'BRACKET_CLOSE (car tokens))) 
     (reverse statements))
    (else
      (parse-while-statements (cdr tokens) (cons (car tokens) statements)))))
; ----------------------------------------------------------------------------

; Given some tokens, and an AST (which starts as an empty list), find a parser
; using a single look-ahead and apply that parser to the tokens. The parser is
; in charge of returning the ast.
(define (tokens->ast tokens ast)
  (if (null? tokens) 
    (reverse ast)
    (let ((lookahead (car tokens)))
      (cond ((equal? 'PLUS lookahead)
             (parse-increment tokens ast))
            ((equal? 'MINUS lookahead)
             (parse-decrement tokens ast))
            ((equal? 'GREATER_THAN lookahead)
             (parse-advance tokens ast))
            ((equal? 'SMALLER_THAN lookahead)
             (parse-back tokens ast))
            ((equal? 'DOT lookahead)
             (parse-output tokens ast))
            ((equal? 'COMMA lookahead)
             (parse-input tokens ast))
            ((equal? 'BRACKET_OPEN lookahead)
             (parse-while tokens ast))
            (else 
              (print "Invalid token:" lookahead))))))

; Prettier version of `tokens->ast`
(define (parse tokens)
  (tokens->ast tokens '()))
