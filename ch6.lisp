; ch6-1
(defun tokens-6-1 (str &key (test #'constituent) (start 0)) ;modified  
  (let ((p1 (position-if test str :start start)))
    (if p1
        (let ((p2 (position-if-not test str :start p1)))
          (cons (subseq str p1 p2)
                (if p2
                    (tokens-6-1 str :test test :start p2)))))))

; ch6-2
(defun bin-search-6-2 (obj vec &key (key #'identity) (test #'eql) (start 0) end)
  (let ((len (or end (length vec))))
    (and (not (zerop len))
         (find obj vec start (- len 1) key test))))

(defun finder (obj vec start end key test)
  (let ((range (- end start)))
    (if (zerop range)
        (if (funcall test obj (funcall key (aref vec start)))
            (aref vec start))
        (let ((mid (+ start (round (/ range 2)))))
          (let ((obj2 (funcall key (aref vec mid))))
            (if (< obj obj2)
                (finder obj vec start (- mid 1) key test)
                (if (> obj obj2)
                    (finder obj vec (+ mid 1) end key test)
                    (aref vec mid))))))))

; ch6-3
(defun return-any (&rest args)
  args)

; ch6-4
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
             (max (funcall fn wins)))
        (dolist (obj (cdr lst))
          (let ((score (funcall fn obj)))
            (when (> score (funcall fn wins)) ;modified
                  (setf max wins ;modified
                    wins obj)))) ;modified
        (values wins max))))

; ch6-5
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))
(defun our-remove-if (fn lst)
  (filter #'(lambda (x) (and (not (funcall fn x)) x)) lst))

; ch6-6
(let (mx)
  (defun max-so-far (n)
    (if (or (not mx) (< mx n))
        (setf mx n)
        mx)))

; ch6-7
(let (prev)
  (defun greater-p (n)
    (prog1
        (and prev (< prev n))
      (setf prev n))))

; ch6-8
; nil

; ch6-9
(defun print8 (&rest args)
  (let ((*print-base* 8))
    (apply #'apply args)))