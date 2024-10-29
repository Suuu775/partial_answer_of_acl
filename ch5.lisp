; ch5-1
; (labels ((get-x () (car y)))
;   (let ((x (get-x)))
;     (cons x x)))
; (labels ((get-w () (car x)))
;   (let ((w (get-w)))
;     (let ((y (+ w z)))
;       (cons w y))))

; ch5-2
(defun mystery (x y)
  (cond
   ((null y) nil)
   ((eql (car y) x) 0)
   (t
     (let ((z (mystery x (cdr y))))
       (and z (+ z 1))))))

; ch5-3
(defun square (n)
  (cond
   ((< n 0) (error "n < 0"))
   ((<= n 5) n)
   (t (* n n))))

; ch5-4
(defun month-num (m y)
  (+ (case m
       (1 0)
       (2 31)
       (3 59)
       (4 90)
       (5 120)
       (6 151)
       (7 181)
       (8 212)
       (9 243)
       (10 273)
       (11 304)
       (12 334)
       (t (error "Invalid month")))
     (if (and (> m 2) (leap? y)) 1 0)))

(defun leap? (y)
  (and (zerop (mod y 4))
       (or (zerop (mod y 400))
           (not (zerop (mod y 100))))))

; ch5-5
(defun precedes-rec (x v) ; wrong !!!
  (let
      ((res nil))
    (cond
     ((equal 0 (position x v)) (precedes-rec x (subseq v 1)))
     ((eq nil (position x v)) res)
     (t (unless (member (char v (1- (position x v))) res)
          (progn
           (push (char v (1- (position x v))) res)
           (precedes-rec x (subseq v (1+ (position x v))))))))))

(defun precedes-iterative (x v)
  (let ((result '())
        (found nil))
    (dotimes (i (length v))
      (let ((char (char v i)))
        (if (eq char x)
            (setf found t)
            (unless found
              (push char result)))))
    (nreverse result)))

; ch5-6
(defun intersperse (obj lst)
  (cond
   ((eq (length lst) 0) (error "empty list"))
   ((eq (length lst) 1) lst)
   (t (cons (car lst) (cons obj (intersperse obj (cdr lst)))))))

; ch5-7
(defun check-difference-recursive (numlst)
  (cond
   ((or (null numlst) (null (cdr numlst))) t)
   ((= (abs (- (car numlst) (cadr numlst))) 1)
     (check-difference-recursive (cdr numlst)))
   (t nil)))

(defun check-difference-do (numbers)
  (do ((lst numbers (cdr lst)))
      ((or (null lst) (null (cdr lst))) t)
    (when (and (not (null (cdr lst)))
               (not (= (abs (- (car lst) (cadr lst))) 1)))
          (return nil))))

; ch5-8
(defun max-min (lst)
  (cond
   ((null lst) (error "empty list"))
   ((null (cdr lst)) (values (car lst) (car lst)))
   (t (max-min-help (cdr lst) (car lst) (car lst)))
   ))
(defun max-min-help (lst max min)
  (cond
   ((null lst) (values max min))
   (t (max-min-help (cdr lst) 
                    (if (> (car lst) max) (car lst) max)
                    (if (< (car lst) min) (car lst) min)
                    ))))

; ch5-9
; nil