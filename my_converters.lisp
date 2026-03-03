
;; This converter shows the traversal order--it puts subexpressions whose head operator
;; is +,*, or ^ in a labeled box numbered consecutively.
(defmvar *box-counter* 0)    
(define-function-converter ((:algebraic $boxed) ($algebraic $boxed)) (op x)
  (ftake 'mlabox (fapply op x) (incf *box-counter* 1)))
