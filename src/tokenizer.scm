(declare (unit tokenizer))
(declare (uses string-helpers))

; Let's tokenize BRAINFUCK
(define (tokenize input)
  (let ((head (string-head input))
        (tail (string-tail input)))
    (cond
      ((null? head)
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
        (cons 'BRACKET_CLOSE(tokenize tail)))
      ; Ignore all other characters
      (else 
        (tokenize tail)))))

;(tokenize "><n>asd")
