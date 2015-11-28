(declare (unit string-helpers))

(define (string-tail str)
  (let ((chars (string->list str)))
    (cond ((null? chars) "")
          (else (list->string (cdr chars))))))

(define (string-head str)
  (cond ((equal? str "") '())
        (else (make-string 1 (car (string->list str))))))
