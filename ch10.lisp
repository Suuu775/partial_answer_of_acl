; ch10-1
; (let((x 'a)
;      (y 'b)
;      (z '(c d)))
;     `(,z ,x z))
; (let((x 'a)
;      (y 'b)
;      (z '(c d)))
;     `(x ,y ,@z))

; (let((x 'a)
;      (y 'b)
;      (z '(c d)))
;     `((,@z ,x) z))

; ch10-2
(defmacro hand-if (test x &optional y)
    `(cond
      (,test ,x)
      ((not ,test) ,y)))

; ch10-3
(defmacro nth-expr (n &rest args)
  `(nth ,n (list ,@args)))

; ch10-4&5&6&7
; nil

; ch10-8
(defmacro double1 (n)
  `(cond
    ((typep ,n 'integer) (* ,n 2))
    ((typep ,n 'list) (append ,n ,n))
    (t other) ;blah blah blah
    ))