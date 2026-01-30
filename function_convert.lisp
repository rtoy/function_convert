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

(defmfun $list_converters (&rest names)
  (let* ((normalized (mapcar #'$nounify names)) (results nil))
    (dolist (entry (list-converters))
      (destructuring-bind ((from . to) fn doc) entry
        (declare (ignore fn))
        (when (or (endp names)
                  (member ($nounify from) normalized :test #'equal))
          (mtell "~M => ~M : ~M ~%" from to doc)
          ;; Accumulate a Maxima-style list entry
          (push (ftake '$=> from to) results))))
    ;; Return results in forward order
    (fapply 'mlist (nreverse results))))


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

;; These binding powers makes a+b => c parse as (a+b) => c; it also makes '=>' left associative.
($infix "=>" 80 80)

#| 
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
                  (or (symbolp (third x)) (stringp (third x)) (lambda-p (third x))))
                (merror "Bad transformation ~M" x))))
    ;; check that the arguments in fun-subs-list are legitimate.
    (every #'check-subs fun-subs-list)
    (dolist (q fun-subs-list)
      (setq e (function-convert e (fn (second q)) (fn (third q)))))
    e))
|#

(defmfun $function_convert (subs e)
  (let ((fun-subs-list (if ($listp subs) (cdr subs) (list subs))))
    (flet ((fn (x)
             (cond ((stringp x) ($verbify x))
                   ((lambda-p x) x)
                   ;(t ($nounify x))))
                   (t x)))

           (check-subs (x)
             (cond
               ((not (consp x))
                (merror "Bad transformation (a mapatom): ~M" x))
               ((not (eq (caar x) '$=>))
                (merror "Bad transformation (missing =>): ~M" x))
               ((not (or (symbolp (second x))
                         (stringp (second x))))
                (merror "Bad transformation (invalid LHS): ~M" x))
               ((not (or (symbolp (third x))
                         (stringp (third x))
                         (lambda-p (third x))))
                (merror "Bad transformation (invalid RHS): ~M" x))
               (t t))))
      ;; check that the arguments in fun-subs-list are legitimate.
      (mapc #'check-subs fun-subs-list)
      (dolist (q fun-subs-list)
        (setq e (function-convert e (fn (second q)) (fn (third q)))))
      e)))

(defun function-convert (e op-old op-new)
  ;;;; (print `(e = ,e old = ,op-old new = ,op-new))
   (cond (($mapatom e) e)
         ;; Case I: both op-old & op-new are symbols. For this case, look up the 
         ;; transformation in the *function-convert-hash* hashtable.
         ((and (consp e)
               (or (eq (caar e) op-old) (eq (caar e) (car (get op-old 'mheader))))
               (symbolp op-new)
               ;; bind converter fn inside conjunction--it's OK!
               (let ((fn (or (lookup-converter op-old op-new) 
                             (lookup-converter ($verbify op-old) op-new)
                             (lookup-converter ($nounify op-old) op-new)
                             (lookup-converter (car (get op-old 'mheader)) op-new))))
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

;; Debugging Hint: If you define a converter that doesn't trigger correctly, try
;; tracing lookup-converter and look at the output of list_converters.
;; In define-converter, don't quote the source and target functions.
(define-converter (%sinc %sin) (x)
  "Convert sinc(x) into sin(x)/x."
  (let ((z (car x)))
    (div (ftake '%sin z) z)))

(define-converter (%sinc %gamma) (x)
  "Convert sinc(x) into 1/((gamma(1+x/%pi))*gamma(1-x/%pi))."
  (let ((z (div (car x) '$%pi)))
    (div 1 (mul (ftake '%gamma (add 1 z)) (ftake '%gamma (sub 1 z))))))

(define-converter (%sin %sinc) (x)
  "Convert sin(x) into x*sinc(x)."
  (let ((z (car x)))
    (mul z (ftake '%sinc z))))

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

(define-converter (%sinh %exp) (x)
"Convert sinh(x) to exponential form."
  (let ((z (car x)))
    (div
      (sub (ftake '%exp z)
           (ftake '%exp (neg z)))
      2)))

(define-converter (%cosh %exp) (x)
"Convert cosh(x) to exponential form."
  (let ((z (car x)))
    (div
      (add (ftake '%exp z)
           (ftake '%exp (neg z)))
      2)))

;; tanh → sinh/cosh
(define-converter (%tanh %sinh) (x)
"Convert tanh(x) to sinh(x)/cosh(x)."
  (let ((z (car x)))
    (div (ftake '%sinh z)
         (ftake '%cosh z))))

;; double_factorial → gamma
(define-converter (%genfact %gamma) (x)
"Convert x!! to gamma form."
  (let ((a (car x)) (b (cadr x)) (c (caddr x))) ($makegamma (ftake '%genfact a b c))))
    
;; log10(x) → log(x)/log(10)
(define-converter ($log10 %log) (x)
  "Convert log10(x) into log(x)/log(10)."
  (let ((z (car x)))
    (div (ftake '%log z)  (ftake '%log 10))))

;; I could do logarc transformations, but for now, let's not.

(define-converter (%binomial mfactorial) (x)
"Convert binomial(n,k) to factorial form."
  (let ((n (car x))
        (k (cadr x)))
    (div (ftake 'mfactorial n)
         (mul (ftake 'mfactorial k)
              (ftake 'mfactorial (sub n k))))))

 ;;"!" => product does n! => product(%g23,%g23,1,n)
 (define-converter (mfactorial $product) (x)
  "Convert n! to product(g,g,1,n)."
  (let ((z (car x)) (g ($gensym)))
    (ftake '%product g g 1 z)))

(define-converter (%atan %log) (x)
  "Convert tan(x) to logarc form."
  (let ((z (car x)))
    ($logarc (ftake '%atan z))))

 (define-converter (%gamma_incomplete %expand) (x)
  (let ((a (car x)) (z (cadr x)) ($gamma_expand t))
    (ftake '%gamma_incomplete a z)))
   
;; I'm not sure this is worthwhile--it differs from simply calling trigreduce by the way it handles
;; negative powers. And this rule shows that to do things like sin(x)^2 => (1-cos(2 x))/2 the source 
;; function must be mexpt, not a trigonometric function.

;; All the business about the gensym is to prevent non-trig functions from expanding (well, that's my claim).
(define-converter (mexpt $trigreduce) (x)
 "Convert integer powers of trig to a Fourier sum"
  (let ((z (car x)) (n (cadr x)))
    (cond ((and (consp z) (consp (car z)) (trigp (caar z)) (integerp n))
             ;($trigreduce (ftake 'mexpt z n)))
             (let* ((g (gensym)) 
                    (w (sratsimp ($demoivre ($expand ($exponentialize (ftake 'mexpt (ftake (caar z) g) n)))))))
               (maxima-substitute (cadr z) g w)))
          (t (ftake 'mexpt z n)))))

;; erf-like functions


(define-converter (%erfi %erf) (x)
  "Convert erfi(x) into -i * erf(i*x)."
  (let ((z (car x)))
    (mul -1 '$%i (ftake '%erf (mul '$%i z)))))


(define-converter (%erf %erfi) (x)
  "Convert erf(x) into i * erfi(-i*x)."
  (let ((z (car x)))
    (mul '$%i (ftake '%erfi (mul -1 '$%i z)))))

(define-converter (%erf %erfc) (x)
  "Convert erf(x) into 1 - erfc(x)."
  (let ((z (car x)))
    (sub 1 (ftake '%erfc z))))

(define-converter (%erfc %erf) (x)
  "Convert erfc(x) into 1 - erf(x)."
  (let ((z (car x)))
    (sub 1 (ftake '%erf z))))

(define-converter (%erf $integral) (x)
  "Convert erfc(x) into an integral representation"
  (let ((z (car x))
        (s (gentemp "$X" :maxima)))
         (mul (div 2 (ftake 'mexpt '$%pi (div 1 2)))
           (ftake '%integrate  (ftake 'mexpt '$%e (mul -1 s s)) s  0 z))))

(define-converter (%erf %hypergeometric) (x)
  "Convert erf(x) into (2*x/sqrt(pi))*hypergeometric([1/2],[3/2],-x^2)."
  (let ((z (car x))) 
    (let (($hypergeometric_representation t)) (ftake '%erf z))))
  
;; abs
(define-converter (mabs %signum) (x)
  "Convert abs(x) into x*signum(x)."
  (let ((z (car x)))
    (mul z (ftake '%signum z))))

(define-converter (mabs %sqrt) (x)
  "Convert abs(x) into sqrt(x^2). When radexpand is true, this is simplified back to abs(x)"
  (let ((z (car x)))
    (ftake 'mexpt (mul z (ftake '$conjugate z)) (div 1 2))))

(define-converter (%signum %hstep) (x)
  "Convert signum(x) into 2 hstep(x) - 1."
  (let ((z (car x)))
    (sub (mul 2 (ftake '%hstep z)) 1)))

(define-converter (%hstep %signum) (x)
  "Convert hstep(x) into (1+signum(x))/2."
  (let ((z (car x)))
    (div (add 1 (ftake '%signum z)) 2)))

(define-converter (%sin %sin) (x)
  (let ((z (car x)))
    (cond
      (($polynomialp z (ftake 'mlist '$%pi)
                      #'(lambda (s) (freeof '$%pi s))
                      #'(lambda (s) (or (eql 0 s) (eql 1 s))))
       (let* ((n (coeff z '$%pi 1))
              (w (sratsimp (sub z (mul n '$%pi)))))
         (ftake '%sin (add w (reduce-angle-mod-2pi (mul '$%pi n))))))
      (t  (ftake '%sin z)))))

           
      