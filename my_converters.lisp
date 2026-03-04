(defmvar $box_counter 0)    
(define-function-converter ((:algebraic $boxed) ($algebraic $boxed)) (op x)
 "This converter puts subexpressions whose head operator is +,*, or ^ in a labeled box numbered consecutively.
  Other than showing the postorder application of converters, it serves no purpose. The box counter is
 stored in a special variable `box_counter'.  "
  (ftake 'mlabox (fapply op x) (incf $box_counter 1)))


;; This maybe demonstrates that converters should be based on identities--when they are
;; not, the results are predicable, but rubbish.
(define-function-converter ((:algebraic $integrate) ($algebraic $integrate)) (op x)
  (let ((e (fapply op x)))
    ($integrate e '$x)))