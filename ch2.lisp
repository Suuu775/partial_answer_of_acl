; ch2-1
(+ (- 5 1) (+ 3 7)) ;14
(list 1 (+ 2 3)) ;(1 5)
(if (listp 1) (+ 1 2) (+ 3 4)) ;7
(list (and (listp 3) t) (+ 1 2))  ;(nil 3)

; ch2-2
(cons 'a (cons 'b (cons 'c nil)))
(cons 'a (cons 'b '(c)))
(cons 'a '(b c))

; ch2-3
(defun forth (lst)
  (car (cdddr lst)))

; ch2-4
(defun biggrt (a b)
  (if (>= a b) a b ))

; ch2-5
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
           (enigma (cdr x))))) ;if list x have element `nil` return t else return nil

(defun mystery (x y)
      (if (null y)
          nil
          (if (eql (car y) x)
              0
              (let ((z (mystery x (cdr y))))
                (and z (+ z 1)))))) ;The function mystery counts the number of occurrences of element x in list y.

; ch2-6
(car (car (cdr '(a (b c) d))))
(or 13 (/ 1 0))
(funcall  #'list 1 nil)

; ch2-7
(defun anylist (lst)
  (cond
   ((null lst) nil)
   ((typep (car lst) 'list) t)
   (t (anylist (cdr lst)))))

; ch2-8a
(defun printdot (n)
  (cond
   ((eq n 1) (princ "."))
   ((> n 1) (progn (princ ".") (printdot (- n 1))))))

(defun print-dots-iterative (n)
  (dotimes (i n)
    (princ ".")))

; ch2-8b
(defun occur (a lst)
  (cond
   ((null lst) 0)
   ((equal a (car lst)) (+ 1 (occur a (cdr lst))))
   (t (occur a (cdr lst)))
  ))

(defun occur-iter (a lst)
  (let
    ((col 0))
    (dolist (obj lst)
      (if (equal obj a) (setf col (1+ col)))
    )
  col))

; ch2-9
(defun summit (lst)
  (setf lst (remove nil lst))
  (apply #'+ lst))

(defun summit-2 (lst)
    (if (null lst)
      0
      (if (null (car lst))
        (summit-2 (cdr lst))
        (+ (car lst) (summit-2 (cdr lst))))))