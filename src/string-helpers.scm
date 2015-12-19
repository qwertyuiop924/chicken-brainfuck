; Just some helpers for playing with strings
(declare (unit string-helpers))
(use srfi-13) ; string goodies

(define (string-tail str)
  (if (string-null? str)
      ""
      (string-drop str 1)))

(define (string-head str)
  (if (string-null? str)
      ""
      (string-take str 1)))
