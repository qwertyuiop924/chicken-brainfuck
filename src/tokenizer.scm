;; This is the tokenizer. The main function is `tokenize` which takes an input
;; and returns a list of tokens. It ignores non-valid tokens.

(declare (unit tokenizer))
(declare (uses string-helpers))
(use srfi-1 srfi-13 anaphora);string-null?, acond and assoc

(define TOKEN-MAP
  '((">" . GREATER_THAN)
    ("<" . SMALLER_THAN)
    ("+" . PLUS)
    ("-" . MINUS)
    ("." . DOT)
    ("," . COMMA)
    ("[" . BRACKET_OPEN)
    ("]" . BRACKET_CLOSE)
    ("%" . PERCENT)
    ("!" . BANG)))

(define (tokenize input)
  (if (eof-object? input)
      '(EOF)
      (let ((head (string-head input))
            (tail (string-tail input)))
        (acond
         ((string-null? head)
          '())
         ((assoc head TOKEN-MAP)
          (cons (cdr it) (tokenize tail)))
         (else ;Ignore all other characters 
          (tokenize tail))))))
      
