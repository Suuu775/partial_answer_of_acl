; ch9-1
(defun not-descending (ls)
  (not (apply #'>= ls)))

; ch9-2
(defun coins(a)
  (labels ((rec (am coins ncoins)
        (if coins
            (multiple-value-bind (n r) (floor am (car coins))
                                       (rec r (cdr coins) (cons n ncoins)))
          (nreverse (cons am ncoins)))))
    (rec a '(25 10 5) nil)))

; ch9-3&ch9-4&ch9-5
; nil

; ch9-6
(defun horner (x &rest args)
  (let
      ((a (car args)))
    (dolist (obj (cdr args))
      (setf a (+ (* a x)obj)))
    a))

; ch9-7
(log (1+ most-positive-fixnum) 2)