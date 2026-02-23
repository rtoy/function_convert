(define-function-converter (%sinc %sin) (op x)
  :builtin
  "Convert sinc(x) into sin(x)/x."
  (declare (ignore op))
  (let ((z (car x)))
    (div (ftake '%sin z) z)))