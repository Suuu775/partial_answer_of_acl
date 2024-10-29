; ch3-2
(defun new-union (list1 list2)
  (let ((result '()))
    (dolist (item list1)
      (unless (member item result)
        (push item result)))
    (dolist (item list2)
      (unless (member item result)
        (push item result)))
    (nreverse result)))

; ch3-3
(defun occur-iter (a lst)
  (let
    ((col 0))
    (dolist (obj lst)
      (if (equal obj a) (setf col (1+ col)))
    )
  col))

(defun occurrences (lst)
  (let ((count-table (make-hash-table :test 'equal))
        (res nil))
    (dolist (obj lst)
      (incf (gethash obj count-table 0)))
    (maphash (lambda (key value)
               (push (cons key value) res))
             count-table)
    (sort res #'(lambda (x y) (> (cdr x) (cdr y))))))

; ch3-4
; because the function member is use `eq`,instead of `equal` or `equalp`

; ch3-5
(defun pos+-rec-help (lst n)
  (if (null lst)
      nil
      (cons (+ n (car lst)) (pos+-rec-help (cdr lst) (1+ n )))))
(defun pos+-rec (lst)
  (pos+-rec-help lst 0))

(defun pos+-iter (lst)
  (let ((cnt 0)
        (res nil))
    (dolist (obj lst)
      (progn
       (push (+ cnt obj) res)
       (setf cnt (1+ cnt))))
    (nreverse res)))

(defun pos+mapcar (lst)
  (mapcar #'+ lst
    (let ((res nil))
      (dotimes (i (length  lst))
        (push i res))
      (nreverse res))))

; ch3-6
(defun gov-cons (a b)
  (list (car b) (cons a (cdr b))))

(defun gov-list (&rest args)
  (if (null args)
      nil
      (cons (car args) (gov-list (cdr args)))))

(defun gov-length (lst)
  (if (null lst)
      0
      (+ 1 (gov-length (cdr lst)))))

(defun gov-member (item lst)
  (cond ((null lst) nil)
        ((equal item (cdr lst)) lst)
        (t (gov-member item (car lst)))))

; ch3-7
(defun point-cons (a b)
  (if (null b)
      (list a)
      (list (car b) (point-cons a (cdr b)))))

(defun point-list (&rest args)
  (if (null args)
      nil
      (point-cons (car args) (point-list (cdr args)))))

(defun point-length (lst)
  (if (null lst)
      0
      (+ 1 (point-length (cdr lst)))))

(defun point-member (item lst)
  (cond ((null lst) nil)
        ((equal item (cdr lst)) lst)
        (t (point-member item (car lst)))))

(defun point-car (lst)
  (if (null lst)
      nil
      (cdr lst)))

(defun point-cdr (lst)
  (if (null lst)
      nil
      (car lst)))

; ch3-8
(defun showdots (lst)
  (if (null lst)
      (format t "NIL")
      (showdots-helper lst (length lst))))

(defun showdots-helper (lst len)
    (if (null lst)
      (progn
        (princ "NIL")
        (dotimes (i len)
          (princ ")"))
       )
      (progn
         (format t "(~A . " (car lst))
         (showdots-helper (cdr lst) len))))

; ch3-9
; nil