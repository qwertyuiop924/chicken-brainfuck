; This is the tokenizer. The main function is `tokenize` which takes an input
; and returns a list of tokens. It ignores non-valid tokens.
;
(declare (unit tokenizer))
(declare (uses string-helpers))

(define (tokenize input)
  (let ((head (string-head input))
        (tail (string-tail input)))
    (cond
     ((string-null? head)
      '())
     ((equal? ">" head) 
      (cons 'GREATER_THAN (tokenize tail)))
     ((equal? "<" head) 
      (cons 'SMALLER_THAN (tokenize tail)))
     ((equal? "+" head) 
      (cons 'PLUS (tokenize tail)))
     ((equal? "-" head) 
      (cons 'MINUS (tokenize tail)))
     ((equal? "." head) 
      (cons 'DOT (tokenize tail)))
     ((equal? "," head) 
      (cons 'COMMA (tokenize tail)))
     ((equal? "[" head) 
      (cons 'BRACKET_OPEN (tokenize tail)))
     ((equal? "]" head) 
      (cons 'BRACKET_CLOSE (tokenize tail)))
     ((equal? "%" head)
      (cons 'PERCENT (tokenize tail)))
      
      ; Ignore all other characters
      (else 
        (tokenize tail)))))
