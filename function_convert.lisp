(in-package :maxima)

(defun lambda-p (e)
"Return true when `e` is a Maxima lambda form."
  (and (consp e)
       (consp (car e))
       (eq (caar e) 'lambda)))

(defun converter-key (from to)
  "Return a key for the converter registry."
  (cons from to))

(defmvar *function-convert-hash*
  (make-hash-table :test 'equal)
  "Hash table mapping (FROM . TO) operator pairs to converter functions.")

(defun register-converter (from to fn)
  "Register FN as the converter from FROM to TO."
  (setf (gethash (converter-key from to) *function-convert-hash*) fn))

(defun lookup-converter (from to)
  "Return the converter function from FROM to TO, or NIL if none exists."
  (gethash (converter-key from to) *function-convert-hash*))

(defun unregister-converter (from to)
  "Remove any converter registered from FROM to TO."
  (remhash (converter-key from to) *function-convert-hash*))

(defun list-converters ()
  "Return a list of entries describing all registered converters.
Each entry has the form:
  ((FROM . TO) FUNCTION-SYMBOL DOCSTRING)"
  (let (acc)
    (maphash
     (lambda (key fn)
       (push (list key
                   fn
                   (documentation fn 'function))
             acc))
     *function-convert-hash*)
    acc))

(defmfun $list_converters ()
  (dolist (entry (list-converters))
    (destructuring-bind ((from . to) fn doc) entry
      (mtell "~M => ~M : ~M~%" from to doc))))

(defmacro define-converter ((from to) lambda-list &body body)
  "Define a converter from FROM to TO, automatically naming the function
CONVERTER-FROM-TO, installing it in the converter registry, and returning
the function symbol."
  (let* ((fname (intern (format nil "CONVERTER-~A-~A"
                                from
                                to)
                        :maxima)))
    `(progn
       (defun ,fname ,lambda-list
         ,@body)
       (register-converter ',from ',to #',fname)
       ',fname)))

;; Here “=>” indicates a semantic conversion, not a literal renaming. For example, “sinc => sin” does not 
;; mean “replace the name sinc with sin”. Instead, it applies the built‑in identity sinc(x) = sin(x)/x.
;; Specifically

;; (a) f => g where both f and g are symbols means “use the built‑in conversion from f to g.” When there
;;     is no such built-in conversion, do nothing.
;; (b) f => lambda(...) means “use this explicit conversion instead.”

($infix "=>")

(defmfun $function_convert (e &rest fun-subs-list)
  (flet ((fn (x)
         (cond ((stringp x) ($verbify x))
               ((lambda-p x) x)
               (t ($nounify x))))
         (check-subs (x)
            (or (and
                  (consp x)
                  (eq (caar x) '$=>)
                  (or (symbolp (second x)) (stringp (second x)))
                  (or (symbolp (third x)) (lambda-p (third x))))
                (merror "Bad transformation ~M" x))))
    ;; check that the arguments in fun-subs-list are legitimate.
    (every #'check-subs fun-subs-list)

    (dolist (q fun-subs-list)
      (setq e (function-convert e (fn (second q)) (fn (third q)))))
    e))

(defmfun function-convert (e op-old op-new)
   (cond (($mapatom e) e)
         ;; Case I: both op-old & op-new are symbols. For this case, look up the 
         ;; transformation in the *function-convert-hash* hashtable.
         ((and (consp e)
               (eq (caar e) op-old)
               (symbolp op-new)
               ;; bind converter fn inside conjunction--it's OK!
               (let ((fn (lookup-converter op-old op-new)))
                 (and fn
                   (funcall fn (mapcar (lambda (q) (function-convert q op-old op-new)) (cdr e)))))))

        ;; Case II: op-old is a symbol and op-new is a Maxima lambda form
        ((and (consp e)
              (eq (caar e) op-old)
              (lambda-p op-new))
          ($apply op-new (fapply 'mlist (mapcar (lambda (q) (function-convert q op-old op-new)) (cdr e)))))

        (($subvarp (mop e)) ;subscripted function
		      (subfunmake 
		       (subfunname e) 
			       (subfunsubs e) ;don't convert subscripts, but map over arguments
			       (mapcar #'(lambda (q) (function-convert q op-old op-new)) (subfunargs e))))

		    (t (fapply (caar e) 
            (mapcar #'(lambda (q) (function-convert q op-old op-new)) (cdr e))))))


;;; ------------------------------------------------------------
;;; Starter Library of Function Converters for function_convert
;;; ------------------------------------------------------------

(define-converter (%sinc %sin) (x)
  "Convert sinc(x) into sin(x)/x."
  (let ((z (car x)))
    (div (ftake '%sin z) z)))

(define-converter (mfactorial %gamma) (x)
 "Convert x! into gamma(1+x)."
  (let ((z (car x))) (ftake '%gamma (add 1 z))))

(define-converter (%csc %sin) (x)
  "Convert csc(x) into 1/sin(x)."
    (let ((z (car x))) (div 1 (ftake '%sin z))))

;; tan → sin/cos
(define-converter (%tan %sin) (x)
"Convert tan(x) into sin(x)/cos(x)."
  (let ((z (car x)))
    (div (ftake '%sin z)
         (ftake '%cos z))))

(define-converter (%sin %exp) (x)
  "Convert sin(x) to exponential form."
  (let ((z (car x)))
    (div
      (sub (ftake '%exp (mul '$%i z))
           (ftake '%exp (mul (neg '$%i) z)))
      (mul 2 '$%i))))

(define-converter (%cos %exp) (x)
"Convert cos(x) to exponential form."
  (let ((z (car x)))
    (div
      (add (ftake '%exp (mul '$%i z))
           (ftake '%exp (mul (neg '$%i) z)))
      2)))

#| 

;;; ------------------------------------------------------------
;;; Trig → Exponential
;;; ------------------------------------------------------------







;;; ------------------------------------------------------------
;;; Hyperbolic → Exponential
;;; ------------------------------------------------------------

;; sinh → exp
(define-function-converter sinh-to-exp (%sinh %exp) (x)
  (let ((z (car x)))
    (div
      (sub (ftake '%exp z)
           (ftake '%exp (neg z)))
      2)))

;; cosh → exp
(define-function-converter cosh-to-exp (%cosh %exp) (x)
  (let ((z (car x)))
    (div
      (add (ftake '%exp z)
           (ftake '%exp (neg z)))
      2)))

;; tanh → sinh/cosh
(define-function-converter tanh-to-sinh-cosh (%tanh %sinh) (x)
  (let ((z (car x)))
    (div (ftake '%sinh z)
         (ftake '%cosh z))))

;;; ------------------------------------------------------------
;;; Factorial Family
;;; ------------------------------------------------------------



;; double_factorial → gamma (analytic continuation)
;(define-function-converter dfact-to-gamma ($double_factorial $gamma) (x)
;  (let ((z (car x)))
;;    (mul (pow 2 (div z 2))
  ;       (div (ftake '$gamma (add (div z 2) 1))
  ;            (sqrt '$%pi)))))



;;; ------------------------------------------------------------
;;; Logarithm Base Conversions
;;; ------------------------------------------------------------

;; log_b(x) → log(x)/log(b)
(define-function-converter logb-to-log (%logb %log) (x)
  (let ((z (car x))
        (b (cadr x)))
    (div (ftake '%log z)
         (ftake '%log b))))

;; log10(x) → log(x)/log(10)
(define-function-converter log10-to-log (%log10 %log) (x)
  (let ((z (car x)))
    (div (ftake '%log z)
         (ftake '%log 10))))

;;; ------------------------------------------------------------
;;; Inverse Trig → Logarithmic
;;; ------------------------------------------------------------

;; asin → log
(define-function-converter asin-to-log (%asin %log) (x)
  (let ((z (car x)))
    (mul (neg '$%i)
         (ftake '%log
                (add (mul '$%i z)
                     (sqrt (sub 1 (mul z z))))))))

;; atan → log
(define-function-converter atan-to-log (%atan %log) (x)
  (let ((z (car x)))
    (mul (div '$%i 2)
         (ftake '%log
                (div (add '$%i z)
                     (sub '$%i z))))))

;;; ------------------------------------------------------------
;;; Chebyshev Polynomials
;;; ------------------------------------------------------------

;; T_n(x) → cos(n arccos(x))
(define-function-converter chebyshev-t-to-cos (%chebyshev_t %cos) (x)
  (let ((n (car x))
        (z (cadr x)))
    (ftake '%cos (mul n (ftake '%acos z)))))

;; U_n(x) → sin((n+1) arccos(x)) / sqrt(1−x²)
(define-function-converter chebyshev-u-to-sin (%chebyshev_u %sin) (x)
  (let ((n (car x))
        (z (cadr x)))
    (div (ftake '%sin (mul (add n 1) (ftake '%acos z)))
         (sqrt (sub 1 (mul z z))))))

(define-function-converter binomial-to-factorial ($binomial $factorial) (x)
  (let ((n (car x))
        (k (cadr x)))
    (div (ftake '$factorial n)
         (mul (ftake '$factorial k)
              (ftake '$factorial (sub n k))))))

|#