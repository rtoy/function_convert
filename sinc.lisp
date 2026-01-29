(in-package :maxima)

;;undone:  
;;  (c) exponentialize (not sure what I had in mind)

(defun pure-constant-p (e)
  (cond ((mnump e) t)
        ((or (mplusp e) (mtimesp e) (mexptp e)) (every #'pure-constant-p (cdr e)))
        (t nil)))

(defun sinc-float (x)
 "Evaluate the sinc(x) function for float, bigfloat, or complex float or bigfloat input.
  Return nil when x is not one of these types of numbers. Exceptions:
    (a) when x = 0, return 1 even when $numer is false
    (b) when $numer is true and x is a mnump number or a complex mnump number, convert x to
    an IEEE float and perform floating‑point evaluation."
  (cond
    ;; Special case: sinc(0)
    ((zerop1 x)
       (if $numer 1.0
           (add 1 x))) ; the add 1 ... makes sinc(0.0) = 1.0 (not 1) and sinc(0.0b0) = 1.0b0 (not 1)
    (t
     (multiple-value-bind (flag re im)
         (complex-number-p x #'mnump)
       (cond
         ((not flag) nil) ;no floating evaluation possible: return nil
         (t
          ;; when $numer is true, promote re & im to IEEE floats
          (when $numer
            (setq re ($float re)
                  im ($float im)))
          ;; If neither part is float or bigfloat, cannot evaluate: return nil
          (cond
            ((not (or (float-or-bigfloat-p re) (float-or-bigfloat-p im))) nil)
            ;; real case--avoid complex number division. When x is a bigfloat, can't do (/ sin x) x)s
            ((zerop1 im) (let ((z (bigfloat::to re))) 
                (maxima::to (bigfloat::/ (bigfloat::sin z) z))))
            ;; do floating-point complex evaluation; there is 
            ;; no bigfloat::sinc. Maybe there should be, till then:
            (t
             (let ((z (bigfloat::to re im)))
               (maxima::to (bigfloat::/ (bigfloat::sin z) z)))))))))))

(def-simplifier sinc (x) 
   (cond ((zerop1 x) (sinc-float x))
         ((taylorize (mop form) (second form)))
         ((sinc-float x)); bug for sinc(4.3+%i) & bug for sinc(4+%i), numer
         (t               
          (let* ((z (ftake '%sin x)))
              (cond 
                 ;; When sin(x) is a pure constant and x is not zero, return sin(x)/x.
                 ((and z (pure-constant-p z) (eq t (mnqp 0 x)))  (div z x)) 
                 ;; even reflection rule: sinc(-x) = sinc(x)
                 ((great (neg x) x) (ftake '%sinc (neg x))) 
                 ;; sinc nounform return
                 (t (give-up))))))) 
   
;; sinc commutes with the conjugate, so give '%sinc the 'commutes-with-conjugate property
(setf (get '%sinc 'commutes-with-conjugate) t)

(defun simplim%sinc (e x pt)  
 "Return limit(sinc(X),x,pt)."
  (let* ((preserve-direction t) 
         (lim (limit (cadr e) x pt 'think)))
     (cond ((eq lim '$ind) '$ind) ; sinc(ind) = ind
           ((or (eq lim '$minf) (eq lim '$inf)) 0) ; sinc(minf) = 0 & sinc(inf) = 0
           ((zerop2 lim) 1) ; includes zerob & zeroa
           ((or (eq lim '$und) (eq lim '$infinity)) (throw 'limit nil)) ; don't know
           (t (ftake '%sinc lim))))) ; use direct substitution
(setf (get '%sinc 'simplim%function) 'simplim%sinc)

;; Derivative of sinc: x -> (cos(x)-sinc(x))/x
(putprop '%sinc 
	 '((x) ((mtimes) ((mexpt) x -1) ((mplus) ((%cos) x) ((mtimes) -1 ((%sinc) x))))) 'grad)

;; Antiderivative of sinc: x -> expintegral_si(x)
(putprop '%sinc
  '((x) ((%expintegral_si) x)) 'integral)

(defun taylor-sinc (a b)
  "Return a list of dotted pairs (p . q), where p = (2k . 1) and
   q = ((-1)^k . (2k+1)!) for k from 0 to n-1, where n = (floor (car a) 2).

   Each dotted pair q represents a coefficient of the Taylor polynomial
   by (/ (car q) (cdr q)), and p represents the exponent by (/ (car p) (cdr p)).

   The second argument `b` is required by a general scheme used by many
   functions, but not by sinc, so we ignore `b`."
  (declare (ignore b))
  (let ((ord (floor (car a) 2)) (cfs nil) (w 1) (sgn 1))
    (dotimes (k ord)
      (let* ((k2 (* 2 k))
             (p  (cons k2 1))
             (q  (cons sgn w)))
        (push (cons p q) cfs)
        ;; update (-1)^k and (2k+1)!
        (setf sgn (- sgn)
              w   (* w (+ k2 2) (+ k2 3)))))
    (nreverse cfs)))
(setf (get '%sinc 'exp-form) (list (list 'taylor-sinc (cons 1 1)) (cons (cons 0 1) nil)))

;; Power series centered at zero for sinc
(setf (get '%sinc 'sp2)
'((%sum)
 ((mtimes) ((mexpt) -1 *index) ((mexpt) ((mfactorial) ((mplus) 1 ((mtimes) 2 *index))) -1)
  ((mexpt) sp2var ((mtimes) 2 *index)))
 *index 0 $inf))

;; allow rectform(sinc(X)) to work correctly
(defun risplit-sinc (x) ;rectangular form for a signum expression
    (let ((z (cadr x)))
      (risplit (div (ftake '%sin z) z))))
(setf (get '%sinc 'risplit-function) 'risplit-sinc)
(putprop '%sinc 'risplit-sinc 'risplit-function)

(defun sign-sinc (x)
     (let ((z (cadr x)))
      (sign (div (ftake '%sin z) z))))
(putprop '%sinc 'sign-sinc 'sign-function)
;; Tex support for sinc

(defprop %sinc "\\sinc" texword)