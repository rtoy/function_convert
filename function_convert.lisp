(in-package :maxima)

;; Define the infix operator that is used to display a function converter. Thus a converter from
;; function f to function g displays as f *function-convert-infix-op* g. 

;; To be consistent with substitute, we use “=” as the infix operator for semantic conversion. 
;; An arrow-like operator (say “=>”) might clarify the distinction between the source and target functions, 
;; but using “=” is consistent with substitute and it preserves the freedom for users to define “=>” 
;; for other purposes. 

;; The source code comments sometimes use “=>” as the infix operator for a converter. I think this notation 
;; helps distinguish the source and target functions. Anyone wishing to rename the semantic conversion operator 
;; may redefine *function-convert-infix-op*. Example:
;;    ($infix "=>" 80 80)
;;    (defmvar *function-convert-infix-op* '$=>)

;; Here “=” indicates a semantic conversion, not a literal renaming. For example, “sinc = sin” does not 
;; mean “replace the name sinc with sin”. Instead, it applies the built‑in identity sinc(x) = sin(x)/x.
;; Specifically

;; (a) f = g where both f and g are symbols means “use the built‑in conversion from f to g.” When there
;;     is no such built-in conversion, do nothing.
;; (b) f = lambda(...) means “use this explicit conversion instead.”

(defmvar *function-convert-infix-op* 'mequal)

(defun lambda-p (e)
"Return true when `e` is a Maxima lambda form."
  (and (consp e)
       (consp (car e))
       (eq (caar e) 'lambda)))

(defun converter-key (from to)
  "Return a key for the converter registry."
  (cons from to))

(eval-when (:compile-toplevel :load-toplevel :execute)
;; Each converter is stored in the hashtable *function-convert-hash*.
(defmvar *function-convert-hash*
  (make-hash-table :test 'equal)
  "Hash table mapping (FROM . TO) operator pairs to converter functions.")

;; We allow a converter to have an alias. Each alias is stored in the hashtable 
;; *function-convert-hash-alias*. Sometimes the code needs to lookup the reverse
;; alias--we store these in the hashtable *function-convert-hash-reverse-alias*
(defmvar *function-convert-hash-alias*
  (make-hash-table :test 'equal)
  "Hash table mapping (FROM . TO) operator pairs to converter functions.")

(defmvar *function-convert-hash-reverse-alias*
  (make-hash-table :test 'equal)
  "Hash table mapping (FROM . TO) operator pairs to converter functions."))

(defun register-converter (from to fn)
  "Register FN as the converter from FROM to TO."
  (setf (gethash (converter-key from to) *function-convert-hash*) fn))

;; When a converter alias is registered, also register its reverse alias.
(defun register-converter-alias (from to from-alt to-alt)
  (setf (gethash (converter-key from to) *function-convert-hash-alias*) (converter-key from-alt to-alt))
  (setf (gethash (converter-key from-alt to-alt) *function-convert-hash-reverse-alias*) (converter-key from to)))

;; The table *converter-class-table* allows for class-based dispatch of a single converter to 
;; handle families of functions such as trigonometric, hyperbolic, inverse trigonometric, 
;; inverse hyperbolic, and logarithmic operators, not expression types. An operator should be
;; a member of only one class. 
(defparameter *converter-class-table*
  '((:trig        . (%sin %cos %tan %sec %csc %cot))
    (:hyperbolic  . (%sinh %cosh %tanh %sech %csch %coth))
    (:inv_trig    . (%asin %acos %atan %asec %acsc %acot))
    (:inv_hyperbolic . (%asinh %acosh %atanh %asech %acsch %acoth))
    (:exp            . (mexpt))
    (:gamma_like     . (%gamma %beta %binomial %double_factorial mfactorial $pochhammer))
    (:bessel  . (%bessel_j %bessel_y %bessel_i %bessel_k %hankel_1 %hankel_2 %airy_ai %airy_bi %airy_dai %airy_dbi ))
    (:algebraic . (mplus mtimes mexpt))
    (:inequation . (mequal mlessp mleqp mnotequal mgreaterp mgeqp $notequal $equal))
    (:logarithmic . (%log)))
  "Mapping from class keys to lists of operator symbols.")

;; Look up the class key associated with a given operator symbol.
;; This function performs the inverse query of *converter-class-table*:
;; given an operator such as %sin or %log, return the class keyword
;; (:trig, :logarithmic, etc.) whose operator list contains it.
;; Returns NIL if the operator does not belong to any registered class.

;; Operators should be in at most one class key. If it's in more than one key, 
;; this function returns the "first" class that it finds. 
(defun converter-class-of (op)
  "Return the class key for OP, or NIL if OP belongs to no class."
  (dolist (entry *converter-class-table* nil)
    (when (member op (cdr entry) :test #'eq)
      (return (car entry)))))

(defun list-converter-aliases ()
  "Print all alias mappings stored in *function-convert-hash-alias*."
  (maphash
   (lambda (alias-key primary-key)
     (destructuring-bind (from-alt . to-alt) alias-key
       (destructuring-bind (from . to) primary-key
         (format t "~A → ~A   aliases   ~A → ~A~%"
                 from-alt to-alt from to))))
   *function-convert-hash-alias*))

;; This code doesn't look up aliases. The alias lookup happens before this code is called.
(defun lookup-converter (operator op-old op-new)
  "Return a converter op-old => op-new function for expression whose operator is OPERATOR. Tries exact match, noun/verb, and class-key match.
   Generally, operator = op-old (or op-old either noun or verbified) but not for a class-key match."

  (flet
      ;; Try to find a converter for FROM => OP-NEW
      ((try (from) (gethash (cons from op-new) *function-convert-hash*)))
    
    (or
     ;; 1. exact operator
     (try operator)

     ;; 2. noun/verb variants of operator
     (try ($nounify operator))
     (try ($verbify operator))

     ;; 3. legacy: op-old itself
     (and (eq operator op-old)
          (try op-old))

     ;; 4. class-key match
     (let ((class (converter-class-of operator)))
       (when class
         (try class))))))

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
  (let* ((normalized (mapcar #'$nounify names))
         (results nil)
         ($lispdisp nil))
    (dolist (entry (list-converters))
      (destructuring-bind ((from . to) fn doc) entry
        (declare (ignore fn))
        (multiple-value-bind (from-alias to-alias)
            (lookup-converter-reverse-alias from to)
          (when (or (endp names)
                    (member ($nounify from-alias) normalized  :test #'equal))
            (labels
                ((stringify (s)
                   ;; Convert a Maxima symbol to a string.  Using
                   ;; aformat works except it always seems to append a
                   ;; newline.  Remove that before returning the
                   ;; result.
                   (string-right-trim '(#\newline)
                                      (aformat nil "~M" s)))
                 (stringify-op (op)
                   ;; Convert the function converter op to something printable.
                   (case op
                     (mequal "=")
                     (t
                      ;; Hope this does something sensible.
                      (stringify op)))))
              (cond ((zerop (count #\newline doc))
                     ;; One line in the docstring.  Try to wrap
                     ;; everything neatly.  The fancy ~{ part is
                     ;; stolen from PRINT-HELP-STRING.
                     (format t "~A ~A ~A : ~{~<~%    ~1,80:; ~A~>~^~}~%"
                             (stringify from-alias)
                             (stringify-op *function-convert-infix-op*)
                             (stringify to-alias)
                             (pregexp::pregexp-split "\\s+" (or doc ""))))
                    (t
                     ;; More than one line.
                     (format t "~A ~A ~A :"
                             (stringify from-alias)
                             (stringify-op *function-convert-infix-op*)
                             (stringify to-alias))
                     ;; Some docstrings have a newline but don't start
                     ;; with a newline.  Force a newline in this case.
                     (when (char/= #\newline (aref doc 0))
                       (terpri))
                     ;; Print each line of the docstring indented by
                     ;; INDENT spaces.
                     (let ((indent 4))
                       (do* ((start 0 (1+ end))
                             (end (position #\newline doc :start start)
                                  (position #\newline doc :start start)))
                            ((null end)
                             (format t "~VT~A~%" indent (subseq doc start)))
                         (format t "~VT~A~%" indent (subseq doc start end)))))))
            ;; Accumulate a Maxima-style list entry
            (push (ftake *function-convert-infix-op* from-alias to-alias) results)))))
    ;; Return results in sorted order
    ($sort (fapply 'mlist (nreverse results)))))

(defmacro define-function-converter (spec lambda-list &body body)
  "Define a converter FROM => TO, optionally with an alias FROM-ALT => TO-ALT.

A warning is issued if a converter for (FROM . TO) is already defined,
or if an alias (FROM-ALT . TO-ALT) is already present in
*function-convert-hash-alias*."
  
  (macrolet 
      ((warn-if-existing-primary (from to)
       `(when (gethash (cons ,from ,to) *function-convert-hash*)
          (warn (format nil "Converter for ~A ~A ~A is already defined."
                        (stripdollar (string-downcase (symbol-name ,from)))
                        (get *function-convert-infix-op* 'op)
                        (stripdollar (string-downcase (symbol-name ,to)))))))
     (warn-if-existing-alias (from-alt to-alt)
       `(when (gethash (cons ,from-alt ,to-alt)
                       *function-convert-hash-alias*)
          (warn (format nil "Alias converter for ~A ~A ~A is already defined."
                        (stripdollar (string-downcase (symbol-name ,from-alt)))
                        (get *function-convert-infix-op* 'op)
                        (stripdollar (string-downcase (symbol-name ,to-alt))))))))
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
              ,(warn-if-existing-primary from to)
              ,(warn-if-existing-alias from-alt to-alt)
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
              ,(warn-if-existing-primary from to)
              (defun ,fname ,lambda-list
                ,@body)
              (register-converter ',from ',to #',fname)
              ',fname))))

      (t
       (error "Malformed converter spec: ~S" spec)))))

(defun find-conversion-path (src dst)
  "Find a shortest conversion path from SRC to DST.

The search is performed using breadth-first traversal of *FUNCTION-CONVERT-HASH*,
whose keys are conses of the form (FROM . TO). Each node is visited at most once.
Returns a list of nodes starting at SRC and ending at DST, or NIL if no path is
found."

  (multiple-value-bind (src dst)
            (lookup-converter-alias src dst)
            (format t "alias: src=~S  dst=~S~%" src dst)
  (let ((visited (make-hash-table :test 'eq)))
    (setf (gethash src visited) t)
    (labels ((successor-paths (path node)
               (let (paths)
                 (maphash
                  (lambda (key fn)
                    (declare (ignore fn))
                    (let ((from (car key))
                          (to   (cdr key)))
                      (when (and (eq from node)
                                 (not (gethash to visited)))
                        (setf (gethash to visited) t)
                        (push (append path (list to)) paths))))
                  *function-convert-hash*)
                 (nreverse paths)))

             (step (queue)
               (when queue
                 (let* ((path (car queue))
                        (node (car (last path)))
                        (rest (cdr queue)))
                   (when (eq node dst)
                     (return-from step path))
                   (step
                    (append rest
                            (successor-paths path node)))))))
      (step (list (list src)))))))

(defun apply-path (expr path)
 " The second argument PATH is a list like (f g h k), meaning f→g, g→h, h→k. Apply 
   these converters (left to right) to the expression expr."
  (if (or (null path)
          (null (cdr path)))
      expr
      (let* ((from (car path))
             (to (cadr path))
             (expr2 (function-convert expr from to)))
        (apply-path expr2 (cdr path)))))

;; The user-level function. The first argument `subs` must either be a single converter 
;; or a Maxima list of converters; for example function_convert(sinc = sin, XXX) or 
;; function_convert([sinc = sin, sin = exp], XXX). This code checks the validity of the
;; first argument.
(defmfun $function_convert (subs e)
  
  (let ((fun-subs-list (if ($listp subs)
                           (cdr subs)
                           (list subs))))
    (flet ((fn (x)
             (cond ((stringp x) ($verbify x))
                   ((lambda-p x) x)
                   ;; formerly ($nounify x)
                   (t x)))

           (check-subs (x)
             (cond
               ((not (consp x))
                (merror "Bad transformation (a mapatom): ~M" x))

               ((not (eq (caar x) *function-convert-infix-op*))
                (merror "Bad transformation (missing ~M): ~M"
                        *function-convert-infix-op* x))

               ((not (or (symbolp (second x))
                         (stringp (second x))))
                (merror "Bad transformation (invalid LHS): ~M" x))

               ((not (or (symbolp (third x))
                         (stringp (third x))
                         (lambda-p (third x))))
                (merror "Bad transformation (invalid RHS): ~M" x))

               (t t))))

      ;; 1. Validate all substitutions
      (mapc #'check-subs fun-subs-list)
      ;; When lookup-coverter-alias fails, send the conversion to apply-path. The function
      ;; apply-path does a BFS to find a chain of converters.
      (dolist (q fun-subs-list)
        (multiple-value-bind (aa bb)
            (lookup-converter-alias (fn (second q)) (fn (third q)))
            (if (and aa bb)
               (setq e (function-convert e aa bb))
               (setq e (apply-path (ftake *function-convert-infix-op* aa bb) e)))))
     e)))

(defun apply-path (sub e)
  (let* ((f (cadr sub))
         (g (caddr sub))
         (ff (or (converter-class-of (mop e)) f))
         (path (find-conversion-path ff g)))
    (cond
      (($mapatom e) e)
      ;; When path has one or fewer members, convert the arguments of e and apply
      ;; the operator of e.
      ((null (cdr path)) 
        (fapply (mop e) (mapcar #'(lambda (q) (function-convert q ff g)) (cdr e))))
      (t
       (let ((from (pop path)))
         (while path
           (let ((to (pop path)))
             (setq e (function-convert e from to))
             (setq from to))))
       e))))

(defun function-convert (e op-old op-new)
   (cond (($mapatom e) e)
         ;; Case I: both op-old & op-new are symbols. For this case, look up the 
         ;; transformation in the *function-convert-hash* hashtable.
         ((and (consp e)
               (symbolp op-new)
               (symbolp op-old)
               ;; bind converter fn inside conjunction--it's OK!
               (let ((fn (lookup-converter (caar e) op-old op-new)))
                 (and fn
                   (funcall fn (caar e) (mapcar (lambda (q) (function-convert q op-old op-new)) (cdr e)))))))
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

;;; Starter Library of Function Converters for function_convert
;;; ------------------------------------------------------------

;; Debugging Hint: If you define a converter that doesn't trigger correctly, try
;; tracing lookup-converter and look at the output of list_converters.
;; In define-function-converter, don't quote the source and target functions.

(define-function-converter (%sinc %sin) (op x)
  "Convert sinc(x) into sin(x)/x."
  (declare (ignore op))
  (let ((z (car x)))
    (div (ftake '%sin z) z)))

(define-function-converter (%sinc %gamma) (op x)
  "Convert sinc(x) into 1/((gamma(1+x/%pi))*gamma(1-x/%pi))."
   (declare (ignore op))
  (let ((z (div (car x) '$%pi)))
    (div 1 (mul (ftake '%gamma (add 1 z)) (ftake '%gamma (sub 1 z))))))

(define-function-converter (%sin %sinc) (op x)
  "Convert sin(x) into x*sinc(x)."
  (declare (ignore op))
  (let ((z (car x)))
    (mul z (ftake '%sinc z))))

(define-function-converter (%csc %sin) (op x)
  "Convert csc(x) into 1/sin(x)."
   (declare (ignore op))
   (let ((z (car x))) (div 1 (ftake '%sin z))))

;; tan → sin/cos
(define-function-converter (%tan %sin) (op x)
"Convert tan(x) into sin(x)/cos(x)."
  (declare (ignore op))
  (let ((z (car x)))
    (div (ftake '%sin z)
         (ftake '%cos z))))

;; tanh → sinh/cosh
(define-function-converter (%tanh %sinh) (op x)
"Convert tanh(x) to sinh(x)/cosh(x)."
  (declare (ignore op))
  (let ((z (car x)))
    (div (ftake '%sinh z)
         (ftake '%cosh z))))

(define-function-converter (%genfact $pochhammer) (op x)
  (declare (ignore op))
  (let ((a (car x)) (b (cadr x)) (c (caddr x)))
    (div (ftake 'mexpt c (ftake '$floor b)) (ftake '$pochhammer (add 1 (div a c)) (ftake '$ceiling (neg b))))))

;; log10(x) → log(x)/log(10)
(define-function-converter ($log10 %log) (op x)
  "Convert log10(x) into log(x)/log(10)."
  (declare (ignore op))
  (let ((z (car x)))
    (div (ftake '%log z)  (ftake '%log 10))))

;; I could do logarc transformations, but for now, let's not.

(define-function-converter (%binomial mfactorial) (op x)
"Convert binomial(n,k) to factorial form."
  (declare (ignore op))
  (let ((n (car x))
        (k (cadr x)))
    (div (ftake 'mfactorial n)
         (mul (ftake 'mfactorial k)
              (ftake 'mfactorial (sub n k))))))

 ;;"!" => product does n! => product(%g23,%g23,1,n)
 (define-function-converter (mfactorial $product) (op x)
  "Convert n! to product(g,g,1,n)."
  (declare (ignore op))
  (let ((z (car x)) (g ($gensym)))
    (ftake '%product g g 1 z)))

(define-function-converter (%atan %log) (op x)
  "Convert tan(x) to logarc form."
  (declare (ignore op))
  (let ((z (car x)))
    ($logarc (ftake '%atan z))))

 (define-function-converter (%gamma_incomplete $expand) (op x)
   (declare (ignore op))
  (let ((a (car x)) (z (cadr x)) ($gamma_expand t))
    (ftake '%gamma_incomplete a z)))
   
;; I'm not sure this is worthwhile--it differs from simply calling trigreduce by the way it handles
;; negative powers. And this rule shows that to do things like sin(x)^2 => (1-cos(2 x))/2 the source 
;; function must be mexpt, not a trigonometric function.

;; All the business about the gensym is to prevent non-trig functions from expanding (well, that's my claim).
(define-function-converter (mexpt $trigreduce) (op x)
 "Convert integer powers of trig to a Fourier sum"
  (declare (ignore op))
  (let ((z (car x)) (n (cadr x)))
    (cond ((and (consp z) (consp (car z)) (trigp (caar z)) (integerp n))
             ;($trigreduce (ftake 'mexpt z n)))
             (let* ((g (gensym)) 
                    (w (sratsimp ($demoivre ($expand ($exponentialize (ftake 'mexpt (ftake (caar z) g) n)))))))
               (maxima-substitute (cadr z) g w)))
          (t (ftake 'mexpt z n)))))

;; erf-like functions
(define-function-converter (%erfi %erf) (op x)
  "Convert erfi(x) into -i * erf(i*x)."
  (declare (ignore op))
  (let ((z (car x)))
    (mul -1 '$%i (ftake '%erf (mul '$%i z)))))

(define-function-converter (%erf %erfi) (op x)
  "Convert erf(x) into i * erfi(-i*x)."
  (declare (ignore op))
  (let ((z (car x)))
    (mul '$%i (ftake '%erfi (mul -1 '$%i z)))))

(define-function-converter (%erf %erfc) (op x)
  "Convert erf(x) into 1 - erfc(x)."
  (declare (ignore op))
  (let ((z (car x)))
    (sub 1 (ftake '%erfc z))))

(define-function-converter (%erfc %erf) (op x)
  "Convert erfc(x) into 1 - erf(x)."
  (declare (ignore op))
  (let ((z (car x)))
    (sub 1 (ftake '%erf z))))

(define-function-converter (%erf $integral) (op x)
  "Convert erfc(x) into an integral representation"
  (declare (ignore op))
  (let ((z (car x))
        (s (gentemp "$X" :maxima)))
         (mul (div 2 (ftake 'mexpt '$%pi (div 1 2)))
           (ftake '%integrate  (ftake 'mexpt '$%e (mul -1 s s)) s  0 z))))

(define-function-converter (%erf %hypergeometric) (op x)
  "Convert erf(x) into (2*x/sqrt(pi))*hypergeometric([1/2],[3/2],-x^2)."
  (declare (ignore op))
  (let ((z (car x))) 
    (let (($hypergeometric_representation t)) (ftake '%erf z))))
  
;; abs
(define-function-converter (mabs %signum) (op x)
  "Convert abs(x) into x*signum(x)."
  (declare (ignore op))
  (let ((z (car x)))
    (mul z (ftake '%signum z))))

(define-function-converter ((:algebraic mabs) (%signum mabs)) (op x)
  "Convert subexpressions of the form X*signum(X) into abs(X).  This converter
does not rewrite signum(X) itself, since X*signum(X) is not a subexpression
of signum(X); only explicit products matching that pattern are transformed."
  (let* ((e  (fapply op x))
         (ll (xgather-args-of e '%signum)))
    (dolist (lx ll)
      (let ((s (car lx)))
        (setq e ($ratsubst (ftake 'mabs s)
                           (mul s (ftake '%signum s))
                           e))))
    ($expand e 0 0)))

(define-function-converter (mabs %sqrt) (op x)
  "Convert abs(x) into sqrt(x^2). When radexpand is true, this is simplified back to abs(x)."
  (declare (ignore op))
  (let ((z (car x)))
    (ftake 'mexpt (mul z (ftake '$conjugate z)) (div 1 2))))

(define-function-converter (%signum %hstep) (op x)
  "Convert signum(x) into 2 hstep(x) - 1."
  (declare (ignore op))
  (let ((z (car x)))
    (sub (mul 2 (ftake '%hstep z)) 1)))

(define-function-converter (%hstep %signum) (op x)
  "Convert hstep(x) into (1+signum(x))/2."
  (declare (ignore op))
  (let ((z (car x)))
    (div (add 1 (ftake '%signum z)) 2)))

(define-function-converter (:trig $normalize_trig_argument) (op x)
 "Normalize the argument of trigonometric functions when the argument
is first degree polynomial in %pi."
  (let ((z (car x))) 
    (cond
      (($polynomialp z (ftake 'mlist '$%pi)
                      #'(lambda (s) (freeof '$%pi s))
                      #'(lambda (s) (or (eql 0 s) (eql 1 s))))
       (let* ((n (coeff z '$%pi 1))
              (w (sratsimp (sub z (mul n '$%pi)))))

         (let ((angle (reduce-angle-mod-2pi (mul '$%pi n))))
           ;; for tan and cot, add %pi to negative angles.
           (when (and (or (eq op '%tan) (eq op '%cot))
                      (eq t (mgrp 0 angle)))
                (setq angle (add angle '$%pi)))
         (ftake op (add w angle)))))
      (t  (ftake op z)))))

;; The function gather-args-of is defined in limit.lisp, but gather-args-of only gathers arguments
;; that involve a specified varible. Here we want to gather all such arguments. The version has
;; predicate for final argument that can be used to exclude arguments. The default for this 
;; predicate is x |-> true. Possibly, this function could replace gather-args-of.
#| 
(defun xgather-args-of (e fn &optional
                           (pred #'(lambda (q) (declare (ignore q)) t))
                           (max-depth nil)
                           (depth 0))
  "Return a list of argument lists for calls to `fn` found in the
expression `e`. Recurses through all subexpressions, applying `pred`
to each candidate call; only those for which `pred` returns true
are included. If `max-depth` is supplied, recursion stops when the
given depth is reached. Results are returned without duplicates,
compared using `alike`."
  (cond
    ;; Stop if max-depth is given and reached.
    ((and max-depth (>= depth max-depth)) nil)

    (($mapatom e) nil)

    (t
     (let* ((subresults
             (reduce #'append
                     (mapcar #'(lambda (q) (xgather-args-of q fn pred max-depth (1+ depth))) (cdr e))
                     :initial-value nil))
            (head
             (and (consp e)
                  (consp (car e))
                  (eq fn (caar e))
                  (funcall pred e)
                  (list (cdr e)))))
       (remove-duplicates (append head subresults) :test #'alike)))))
|#

(defun xgather-args-of (e fn)
   (cond (($mapatom e) nil)        
         ((eq fn (caar e)) (list (cdr e)))
          (t 
        	(remove-duplicates (reduce #'append 
			 (mapcar #'(lambda (q) 
			     (xgather-args-of q fn)) (cdr e))) :test #'alike))))
           
;; Experimental converter for gamma(X)*gamma(1-X) => pi/(sin(pi X)). This must dispatch
;; on a product, not on gamma--this is likely confusing for a user. So we give the 
;; converter an alias of (%gamma %sin). So function_convert(gamma = sin, gamma(X)*gamma(1-X)*a*b*X = 42)
;; will properly trigger the rule.
(define-function-converter ((mtimes %sin) (%gamma %sin)) (op x)
  (declare (ignore op))
  (flet ((gamma-p (s) (and (consp s) (eq (caar s) '%gamma))))
     (let* ((e (fapply 'mtimes x)) (ll (fapply '$set (mapcar #'first (xgather-args-of e '%gamma)))) (ee))
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

;; An example of a converter that uses the class system.
(define-function-converter (:trig $sin_cos) (op x)
 ;; "Convert all six trigonometric functions to sin/cos form."
  (let ((z (first x)))
    (case op
      (%sin (ftake '%sin z))
      (%cos (ftake '%cos z))
      (%tan (div (ftake '%sin z) (ftake '%cos z)))
      (%sec (div 1 (ftake '%cos z)))
      (%csc (div 1 (ftake '%sin z)))
      (%cot (div (ftake '%cos z) (ftake '%sin z)))
      (t (ftake op z)))))

(define-function-converter (:trig %exp) (op x)
 "Convert all trigonometric functions to exponential form."
  ($exponentialize (fapply op x)))

(define-function-converter (:hyperbolic %exp) (op x)
"Convert all hyperbolic functions to exponential form."
  ($exponentialize (fapply op x)))

(define-function-converter (:inverse_trig $log) (op x)
"Convert all inverse trigonometric functions to logarithmic form."
  ($logarc (fapply op x)))

(define-function-converter (:exp :trig) (op x)
  ($demoivre (fapply op x)))

(define-function-converter (:trig $trig_tan_half_angle) (op x)
  "
Rewrite trigonometric functions in terms of the tangent half–angle
substitution t = tan(z/2).  Produces rational functions of t:

    sin(z) → 2 t / (1 + t^2)
    cos(z) → (1 - t^2) / (1 + t^2)
    tan(z) → 2 t / (1 - t^2)
    sec(z) → (1 + t^2) / (1 - t^2)
    csc(z) → (1 + t^2) / (2 t)
    cot(z) → (1 - t^2) / (2 t)

If OP is not one of the standard trigonometric operators, return OP(z)
unchanged.
"
(let* ((z (car x))
       (q (ftake '%tan (div z 2))))
  (case op
    (%sin (div (mul 2 q)
               (add 1 (mul q q))))
    (%cos (div (sub 1 (mul q q))
               (add 1 (mul q q))))
    (%tan (div (mul 2 q)
               (sub 1 (mul q q))))
    (%sec (div (add 1 (mul q q))
               (sub 1 (mul q q))))
    (%csc (div (add 1 (mul q q))
               (mul 2 q)))
    (%cot (div (sub 1 (mul q q))
               (mul 2 q)))
    (t    (ftake op z)))))

(define-function-converter (:hyperbolic $hyperbolic_tanh_half_angle) (op x)
  "
Rewrite hyperbolic functions in terms of the tanh half–angle substitution
u = tanh(z/2).  Produces rational functions of u:

    sinh(z) → 2 u / (1 - u^2)
    cosh(z) → (1 + u^2) / (1 - u^2)
    tanh(z) → 2 u / (1 + u^2)
    sech(z) → (1 - u^2) / (1 + u^2)
    csch(z) → (1 - u^2) / (2 u)
    coth(z) → (1 + u^2) / (2 u)

If OP is not one of the standard hyperbolic operators, return OP(z)
unchanged.
"
  (let* ((z (car x))
         (u (ftake '%tanh (div z 2))))
    (case op
      (%sinh (div (mul 2 u)
                  (sub 1 (mul u u))))
      (%cosh (div (add 1 (mul u u))
                  (sub 1 (mul u u))))
      (%tanh (div (mul 2 u)
                  (add 1 (mul u u))))
      (%sech (div (sub 1 (mul u u))
                  (add 1 (mul u u))))
      (%csch (div (sub 1 (mul u u))
                  (mul 2 u)))
      (%coth (div (add 1 (mul u u))
                  (mul 2 u)))
      (t    (ftake op z)))))

(define-function-converter (:gamma_like %gamma) (op x)
  ($makegamma (fapply op x)))

;; Since gamma(2/3)*gamma(5/3) simplifies to (2/3)*gamma(2/3), this code
;; misses conversion of gamma(2/3)*gamma(5/3)/gamma(7/3), for example.
(define-function-converter ((mtimes %beta) (%gamma %beta)) (op x)
   "Rewrite products of gamma functions into beta functions when possible.
This converter looks for subexpressions of the form
    gamma(a) * gamma(b) / gamma(a + b)
and replaces them with beta(a, b).  Only explicit occurrences of this
three-factor pattern are transformed; the converter does not rewrite
gamma(a) or gamma(b) individually, nor does it attempt to derive beta(a,b)
from expressions where the pattern is not present as an actual
subexpression."
  (let* ((e (fapply op x))
         (ee)
         (ll (mapcar #'car (xgather-args-of e '%gamma)))
         (ga (gensym))
         (gb (gensym))
         (gc (gensym)))
      (dolist (a ll)
         (dolist (b ll)
            (setq ee 
               (maxima-substitute gc (ftake '%gamma (add a b))
                   (maxima-substitute gb (ftake '%gamma b)
                      (maxima-substitute ga (ftake '%gamma a) e))))
            (setq ee ($ratsubst (ftake '%beta a b) (div (mul ga gb) gc) ee))
            (when ($freeof ga gb gc ee)
               (setq e ee))))
      e))

(define-function-converter ((mtimes %binomial) (%gamma %binomial)) (op x)
   "Rewrite products of gamma functions into a binomial coefficient when possible.
This converter looks for subexpressions of the form
    gamma(a+1) /(gamma(b+1)*gamma(a+1-b)
and replaces them with binonl(a, b).  Only explicit occurrences of this
three-factor pattern are transformed; the converter does not rewrite
gamma(a) or gamma(b) individually, nor does it attempt to derive a
binomial coefficientfrom expressions where the pattern is not present as an actual
subexpression."
  (let* ((e (fapply op x))
         (ee)
         (ll (mapcar #'car (xgather-args-of e '%gamma)))
         (ga (gensym))
         (gb (gensym))
         (gc (gensym)))
  
      (dolist (a ll)
         (dolist (b ll)        
            (setq ee 
               (maxima-substitute gc (ftake '%gamma (add 1 a (neg b)))
                   (maxima-substitute gb (ftake '%gamma b)
                      (maxima-substitute ga (ftake '%gamma a) e))))
            (setq ee ($ratsubst (ftake '%binomial (sub a 1) (sub b 1)) (div ga (mul gb gc)) ee))
            (when ($freeof ga gb gc ee)
               (setq e ee))))
      e))

(define-function-converter ((:algebraic $hyperbolic) (%exp %hyperbolic)) (op x)
  (setq x (fapply op x))
  (let ((ll (xgather-args-of x 'mexpt)) (g (gensym)))
    (dolist (lx ll)
      (when (eq '$%e (first lx))
        (let* ((z (second lx)) 
               (ch (ftake '%cosh z)) 
               (sh (ftake '%sinh z))
               (th (div (sub 1 (ftake '%tanh (div z 2))) 2)); th = (1 - tanh(z/2))/2
               (coth (div (sub 1 (ftake '%coth (div z 2))) 2)); ch = (1 - coth(z/2))/2
               (ex (ftake 'mexpt '$%e z)) 
               (xx) (aa) (bb) (cc) (dd))
          (setq xx ($ratsubst g ex x))
          (setq xx ($expand ($partfrac xx g) 1 1))
          (setq aa ($ratsubst (mul 2 ch) (add g (div 1 g)) xx)) ; g + 1/g = 2 ch
          (setq bb ($ratsubst (mul 2 sh) (sub g (div 1 g)) xx)) ; g - 1/g = 2 sh
          ;; Notice that ratsubst(th,1/(1+g),g) => -((th-1)/th). If we accepted this,
          ;; we'd express to many exponentials in terms of tanh. So, we'll use
          ;; maxima-substitute, not ratsubst.        
          (setq cc (maxima-substitute th (div 1 (add 1 g)) xx)) ; 1/(1 + g) = th
          (setq dd (maxima-substitute (neg coth) (div 1 (sub g 1)) xx)) ; 1/(1 - g) = ch
          (cond ((freeof g aa) (setq x aa))
                ((freeof g bb) (setq x bb))
                ((freeof g cc) (setq x cc))
                ((freeof g dd) (setq x dd))))))
    ($trigreduce (resimplify x)))) ; trigreduce does sinh(x)/cosh(x) = tanh(x), for example.
          
  ;; inequations:
 (define-function-converter (:inequation $zero_lhs) (op x)
  "Normalize an inequation `a op b` to the zero LHS form `(a - b) op 0`."
  (destructuring-bind (a b) x
    (ftake op (resimplify ($factor (sub a b))) 0)))


;;; These are toy converters that I used to test find-converter-path. There remains some issues with
;;; find-converter-path used with class keys and aliases. So let's keep these for

(define-function-converter (%a %b) (op x)
  (declare (ignore op))
  (ftake '%b (car x)))

(define-function-converter (%b %c) (op x)
 (declare (ignore op))
  (ftake '%c (car x)))

(define-function-converter (%c %d) (op x)
 (declare (ignore op))
  (ftake '%d (car x)))

(define-function-converter (%d %e) (op x)
 (declare (ignore op))
  (ftake '%e (car x)))

(define-function-converter (%b %e) (op x)
 (declare (ignore op))
 (ftake '%e (car x)))

 (define-function-converter ((%b %d) ($pp $qq)) (op x)
  (declare (ignore op))
  (ftake '%d (car x)))

(defmfun $list_alias ()
  (maphash #'(lambda (a b) (print `(a = ,a b = ,b))) *function-convert-hash-alias*))

(defmfun $list_converters ()
  (maphash #'(lambda (a b) (print `(a = ,a b = ,b))) *function-convert-hash*))