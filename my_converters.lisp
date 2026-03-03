
;; This converter shows the traversal order--it puts subexpressions whose head operator
;; is +,*, or ^ in a labeled box numbered consecutively.
(defmvar *box-counter* 0)    
(define-function-converter ((:algebraic $boxed) ($algebraic $boxed)) (op x)
  (ftake 'mlabox (fapply op x) (incf *box-counter* 1)))


;; This maybe demonstrates that converters should be based on identities--when they are
;; not, the results are predicable, but rubbish.
(define-function-converter ((:algebraic $integrate) ($algebraic $integrate)) (op x)
  (let ((e (fapply op x)))
    ($integrate e '$x)))