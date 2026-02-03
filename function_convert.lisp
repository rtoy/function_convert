(in-package :maxima)

(defmvar *function-convert-infix-op* 'mequal)

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

(defmvar *function-convert-hash-alias*
  (make-hash-table :test 'equal)
  "Hash table mapping (FROM . TO) operator pairs to converter functions.")

(defun register-converter (from to fn)
  "Register FN as the converter from FROM to TO."
  (setf (gethash (converter-key from to) *function-convert-hash*) fn))

(defmvar *function-convert-hash-reverse-alias*
  (make-hash-table :test 'equal)
  "Hash table mapping (FROM . TO) operator pairs to converter functions.")

(defun register-converter-alias (from to from-alt to-alt)
  (setf (gethash (converter-key from to) *function-convert-hash-alias*) (converter-key from-alt to-alt))
  (setf (gethash (converter-key from-alt to-alt) *function-convert-hash-reverse-alias*) (converter-key from to)))

(defun list-converter-aliases ()
  "Print all alias mappings stored in *function-convert-hash-alias*."
  (maphash
   (lambda (alias-key primary-key)
     (destructuring-bind (from-alt . to-alt) alias-key
       (destructuring-bind (from . to) primary-key
         (format t "~A → ~A   aliases   ~A → ~A~%"
                 from-alt to-alt from to))))
   *function-convert-hash-alias*))

(defun lookup-converter (from to)
  "Return the converter function from FROM to TO, or NIL if none exists."
  (gethash (converter-key from to) *function-convert-hash*))

(defun lookup-converter-alias (from to) 
  (let ((primary (gethash (converter-key from to) *function-convert-hash-alias*)))
    (if primary
        (values (car primary) (cdr primary))
        (values from to))))

(defun lookup-converter-reverse-alias (from to) 
  (let ((primary (gethash (converter-key from to) *function-convert-hash-reverse-alias*)))
    (if primary
        (values (car primary) (cdr primary))
        (values from to))))

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
        (multiple-value-bind (from-alias to-alias)
            (lookup-converter-reverse-alias from to)
          (when (or (endp names)
                    (member ($nounify from-alias) normalized  :test #'equal))
            (mtell "~M ~M ~M : ~M ~%" from-alias *function-convert-infix-op* to-alias doc)
            ;; Accumulate a Maxima-style list entry
            (push (ftake *function-convert-infix-op* from-alias to-alias) results)))))
    ;; Return results in sorted order
    ($sort (fapply 'mlist (nreverse results)))))

#| until I and 100% certain the new macro works, let's save the old!

(defmacro define-function-converter ((from to) lambda-list &body body)
  "Define a converter from FROM to TO, automatically naming the function
CONVERTER-FROM-TO, installing it in the converter registry, and returning
the function symbol."
  (let* ((fname (intern (format nil "FUNCTION-CONVERTER-~A-~A"
                                from
                                to)
                        :maxima)))
    `(progn
       (defun ,fname ,lambda-list
         ,@body)
       (register-converter ',from ',to #',fname)
       ',fname)))

(defmacro define-function-converter-alias ((from to) (from-alt to-alt) lambda-list &body body)
  "Define a converter FROM→TO, register it, and record an alias
FROM-ALT→TO-ALT in *function-convert-hash-alias* via REGISTER-CONVERTER-ALIAS."
  (let ((fname (intern (format nil "FUNCTION-CONVERTER-~A-~A"
                               from to)
                       :maxima)))
    `(progn
       (defun ,fname ,lambda-list
         ,@body)
       (register-converter ',from ',to #',fname)
       (register-converter-alias ',from-alt ',to-alt ',from ',to)
       ',fname)))

|#

(defmacro define-function-converter (spec lambda-list &body body)
  "Define a converter FROM→TO, optionally with an alias FROM-ALT→TO-ALT.

Valid forms:
  (define-function-converter (FROM TO) (args) ...)
  (define-function-converter ((FROM TO) (FROM-ALT TO-ALT)) (args) ...)

Registers the converter and, if an alias is supplied, stores the alias
in *function-convert-hash-alias* via REGISTER-CONVERTER-ALIAS and also
stores the reverse mapping via REGISTER-CONVERTER-REVERSE-ALIAS."
  (cond
    ;; Alias form: ((from to) (from-alt to-alt))
    ((and (consp spec)
          (consp (first spec))
          (consp (second spec)))
     (destructuring-bind ((from to) (from-alt to-alt)) spec
       (let ((fname (intern (format nil "FUNCTION-CONVERTER-~A-~A"
                                    from to)
                            :maxima)))
         `(progn
            (defun ,fname ,lambda-list
              ,@body)
            (register-converter ',from ',to #',fname)
            (register-converter-alias ',from-alt ',to-alt ',from ',to)
            ',fname))))

    ;; Simple form: (from to)
    ((and (consp spec)
          (symbolp (first spec))
          (symbolp (second spec)))
     (destructuring-bind (from to) spec
       (let ((fname (intern (format nil "FUNCTION-CONVERTER-~A-~A"
                                    from to)
                            :maxima)))
         `(progn
            (defun ,fname ,lambda-list
              ,@body)
            (register-converter ',from ',to #',fname)
            ',fname))))

    (t
     (error "Malformed converter spec: ~S" spec))))

;; Here “=” indicates a semantic conversion, not a literal renaming. For example, “sinc = sin” does not 
;; mean “replace the name sinc with sin”. Instead, it applies the built‑in identity sinc(x) = sin(x)/x.
;; Specifically

;; (a) f = g where both f and g are symbols means “use the built‑in conversion from f to g.” When there
;;     is no such built-in conversion, do nothing.
;; (b) f = lambda(...) means “use this explicit conversion instead.”

;; To be consistent with substitute, we use “=” as the infix operator for semantic conversion. 
;; Using “=” instead of “=>” preserves the freedom for users to define “=>” for other purposes. 
;; The source code comments use “=>” as the infix operator--I think this notation clarifies the 
;; source and target functions. Anyone wishing to rename the semantic conversion operator 
;; may redefine *function-convert-infix-op*. Example:
;;    ($infix "=>" 80 80)
;;    (defmvar *function-convert-infix-op* '$=>)

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
               ((not (eq (caar x) *function-convert-infix-op*))
                (merror "Bad transformation (missing ~M): ~M" *function-convert-infix-op* x))
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
        (multiple-value-bind (aa bb)
         (lookup-converter-alias (fn (second q)) (fn (third q)))
         (setq e (function-convert e aa bb))))
      e)))

(defun function-convert (e op-old op-new)
   (cond (($mapatom e) e)
         ;; Case I: both op-old & op-new are symbols. For this case, look up the 
         ;; transformation in the *function-convert-hash* hashtable.
         ((and (consp e)
               (member (caar e) (list op-old ($nounify op-old) ($verbify op-old)))
               (symbolp op-new)
               ;; bind converter fn inside conjunction--it's OK!
               (let ((fn (or (lookup-converter op-old op-new) 
                             (lookup-converter ($verbify op-old) op-new)
                             (lookup-converter ($nounify op-old) op-new))))
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
;; In define-function-converter, don't quote the source and target functions.
(define-function-converter (%sinc %sin) (x)
  "Convert sinc(x) into sin(x)/x."
  (let ((z (car x)))
    (div (ftake '%sin z) z)))

(define-function-converter (%sinc %gamma) (x)
  "Convert sinc(x) into 1/((gamma(1+x/%pi))*gamma(1-x/%pi))."
  (let ((z (div (car x) '$%pi)))
    (div 1 (mul (ftake '%gamma (add 1 z)) (ftake '%gamma (sub 1 z))))))

(define-function-converter (%sin %sinc) (x)
  "Convert sin(x) into x*sinc(x)."
  (let ((z (car x)))
    (mul z (ftake '%sinc z))))

(define-function-converter (mfactorial %gamma) (x)
 "Convert x! into gamma(1+x)."
  (let ((z (car x))) (ftake '%gamma (add 1 z))))

(define-function-converter (%csc %sin) (x)
  "Convert csc(x) into 1/sin(x)."
    (let ((z (car x))) (div 1 (ftake '%sin z))))

;; tan → sin/cos
(define-function-converter (%tan %sin) (x)
"Convert tan(x) into sin(x)/cos(x)."
  (let ((z (car x)))
    (div (ftake '%sin z)
         (ftake '%cos z))))

(define-function-converter (%sin %exp) (x)
  "Convert sin(x) to exponential form."
  (let ((z (car x)))
    (div
      (sub (ftake '%exp (mul '$%i z))
           (ftake '%exp (mul (neg '$%i) z)))
      (mul 2 '$%i))))

(define-function-converter (%cos %exp) (x)
"Convert cos(x) to exponential form."
  (let ((z (car x)))
    (div
      (add (ftake '%exp (mul '$%i z))
           (ftake '%exp (mul (neg '$%i) z)))
      2)))

(define-function-converter (%sinh %exp) (x)
"Convert sinh(x) to exponential form."
  (let ((z (car x)))
    (div
      (sub (ftake '%exp z)
           (ftake '%exp (neg z)))
      2)))

(define-function-converter (%cosh %exp) (x)
"Convert cosh(x) to exponential form."
  (let ((z (car x)))
    (div
      (add (ftake '%exp z)
           (ftake '%exp (neg z)))
      2)))

;; tanh → sinh/cosh
(define-function-converter (%tanh %sinh) (x)
"Convert tanh(x) to sinh(x)/cosh(x)."
  (let ((z (car x)))
    (div (ftake '%sinh z)
         (ftake '%cosh z))))

;; double_factorial → gamma
(define-function-converter (%genfact %gamma) (x)
"Convert x!! to gamma form. Set `gamma_expand` to false."
  (let ((a (car x)) (b (cadr x)) (c (caddr x))) ($makegamma (ftake '%genfact a b c))))

(define-function-converter (%genfact $pochhammer) (x)
  (let ((a (car x)) (b (cadr x)) (c (caddr x)))
    (div (ftake 'mexpt c (ftake '$floor b)) (ftake '$pochhammer (add 1 (div a c)) (ftake '$ceiling (neg b))))))

;; log10(x) → log(x)/log(10)
(define-function-converter ($log10 %log) (x)
  "Convert log10(x) into log(x)/log(10)."
  (let ((z (car x)))
    (div (ftake '%log z)  (ftake '%log 10))))

;; I could do logarc transformations, but for now, let's not.

(define-function-converter (%binomial mfactorial) (x)
"Convert binomial(n,k) to factorial form."
  (let ((n (car x))
        (k (cadr x)))
    (div (ftake 'mfactorial n)
         (mul (ftake 'mfactorial k)
              (ftake 'mfactorial (sub n k))))))

 ;;"!" => product does n! => product(%g23,%g23,1,n)
 (define-function-converter (mfactorial $product) (x)
  "Convert n! to product(g,g,1,n)."
  (let ((z (car x)) (g ($gensym)))
    (ftake '%product g g 1 z)))

(define-function-converter (%atan %log) (x)
  "Convert tan(x) to logarc form."
  (let ((z (car x)))
    ($logarc (ftake '%atan z))))

 (define-function-converter (%gamma_incomplete $expand) (x)
  (let ((a (car x)) (z (cadr x)) ($gamma_expand t))
    (ftake '%gamma_incomplete a z)))
   
;; I'm not sure this is worthwhile--it differs from simply calling trigreduce by the way it handles
;; negative powers. And this rule shows that to do things like sin(x)^2 => (1-cos(2 x))/2 the source 
;; function must be mexpt, not a trigonometric function.

;; All the business about the gensym is to prevent non-trig functions from expanding (well, that's my claim).
(define-function-converter (mexpt $trigreduce) (x)
 "Convert integer powers of trig to a Fourier sum"
  (let ((z (car x)) (n (cadr x)))
    (cond ((and (consp z) (consp (car z)) (trigp (caar z)) (integerp n))
             ;($trigreduce (ftake 'mexpt z n)))
             (let* ((g (gensym)) 
                    (w (sratsimp ($demoivre ($expand ($exponentialize (ftake 'mexpt (ftake (caar z) g) n)))))))
               (maxima-substitute (cadr z) g w)))
          (t (ftake 'mexpt z n)))))

;; erf-like functions


(define-function-converter (%erfi %erf) (x)
  "Convert erfi(x) into -i * erf(i*x)."
  (let ((z (car x)))
    (mul -1 '$%i (ftake '%erf (mul '$%i z)))))


(define-function-converter (%erf %erfi) (x)
  "Convert erf(x) into i * erfi(-i*x)."
  (let ((z (car x)))
    (mul '$%i (ftake '%erfi (mul -1 '$%i z)))))

(define-function-converter (%erf %erfc) (x)
  "Convert erf(x) into 1 - erfc(x)."
  (let ((z (car x)))
    (sub 1 (ftake '%erfc z))))

(define-function-converter (%erfc %erf) (x)
  "Convert erfc(x) into 1 - erf(x)."
  (let ((z (car x)))
    (sub 1 (ftake '%erf z))))

(define-function-converter (%erf $integral) (x)
  "Convert erfc(x) into an integral representation"
  (let ((z (car x))
        (s (gentemp "$X" :maxima)))
         (mul (div 2 (ftake 'mexpt '$%pi (div 1 2)))
           (ftake '%integrate  (ftake 'mexpt '$%e (mul -1 s s)) s  0 z))))

(define-function-converter (%erf %hypergeometric) (x)
  "Convert erf(x) into (2*x/sqrt(pi))*hypergeometric([1/2],[3/2],-x^2)."
  (let ((z (car x))) 
    (let (($hypergeometric_representation t)) (ftake '%erf z))))
  
;; abs
(define-function-converter (mabs %signum) (x)
  "Convert abs(x) into x*signum(x)."
  (let ((z (car x)))
    (mul z (ftake '%signum z))))

(define-function-converter (mabs %sqrt) (x)
  "Convert abs(x) into sqrt(x^2). When radexpand is true, this is simplified back to abs(x)"
  (let ((z (car x)))
    (ftake 'mexpt (mul z (ftake '$conjugate z)) (div 1 2))))

(define-function-converter (%signum %hstep) (x)
  "Convert signum(x) into 2 hstep(x) - 1."
  (let ((z (car x)))
    (sub (mul 2 (ftake '%hstep z)) 1)))

(define-function-converter (%hstep %signum) (x)
  "Convert hstep(x) into (1+signum(x))/2."
  (let ((z (car x)))
    (div (add 1 (ftake '%signum z)) 2)))

;; disapointing to have to do this twice ...
(define-function-converter (%sin %sin) (x)
  (let ((z (car x)))
    (cond
      (($polynomialp z (ftake 'mlist '$%pi)
                      #'(lambda (s) (freeof '$%pi s))
                      #'(lambda (s) (or (eql 0 s) (eql 1 s))))
       (let* ((n (coeff z '$%pi 1))
              (w (sratsimp (sub z (mul n '$%pi)))))
         (ftake '%sin (add w (reduce-angle-mod-2pi (mul '$%pi n))))))
      (t  (ftake '%sin z)))))
    
(define-function-converter (%cos %cos) (x)
  (let ((z (car x)))
    (cond
      (($polynomialp z (ftake 'mlist '$%pi)
                      #'(lambda (s) (freeof '$%pi s))
                      #'(lambda (s) (or (eql 0 s) (eql 1 s))))
       (let* ((n (coeff z '$%pi 1))
              (w (sratsimp (sub z (mul n '$%pi)))))
         (ftake '%cos (add w (reduce-angle-mod-2pi (mul '$%pi n))))))
      (t  (ftake '%cos z)))))

;; The function gather-args-of is defined in limit.lisp, but this function only gathers arguments
;; that involve a specified varible. Here we want to gather all such arguments...the function
;; gather-args-of should be extended to take a predicate for inclusion.
(defun xgather-args-of (e fn)
   (cond (($mapatom e) nil)        
         ((and (consp e) (consp (car e)) (eq fn (caar e))) (cdr e))
         (t 
        	(remove-duplicates (reduce #'append 
		 	       (mapcar #'(lambda (q) (xgather-args-of q fn)) (cdr e))) :test #'alike1))))

;; Experimental converter for gamma(X)*gamma(1-X) => pi/(sin(pi X)). This must dispatch
;; on a product, not on gamma--this is likely confusing for a user. So we give the 
;; converter an alias of (%gamma %sin). So function_convert(gamma = sin, gamma(X)*gamma(1-X)*a*b*X = 42)
;; will properly trigger the rule.
(define-function-converter ((mtimes %sin) (%gamma %sin)) (x)
  (flet ((gamma-p (s) (and (consp s) (eq (caar s) '%gamma))))
     (let* ((e (fapply 'mtimes x)) (ll (fapply '$set (xgather-args-of e '%gamma))) (ee))
      (setq ll ($equiv_classes ll #'(lambda (a b) (eql 1 (add a b)))))
      (setq ll (cdr ll))
        (dolist (lx ll)
          (when (eql 2 ($cardinality lx))
            (setq lx (cdr lx)) ;Maxima set to list
             (let* ((g1 (ftake '%gamma (car lx)))  (g2 (ftake '%gamma (cadr lx))) (z))
                ;; set z to the most simple gamma arugment--we'll trust great to choose 
                (if (great (cadr g1) (cadr g2))
                   (setq z (cadr g1))
                   (setq z (cadr g2)))
                (setq ee ($ratsubst (div '$%pi (ftake '%sin (mul '$%pi z))) (mul g1 g2) e))
                ;; when ratsimp eliminates both gamma terms, keep it
                (when (and ($freeof g1 ee) ($freeof g2 ee))
                  (setq e ee)))))
      ($expand e 0 0))))
