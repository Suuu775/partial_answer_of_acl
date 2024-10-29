; ch4-1
(defun quarter-turn (sa)
  (let* ((dimension (array-dimensions sa))
         (rows (first dimension))
         (cols (second dimension))
         (res (make-array (list cols rows))))
    (dotimes (i rows)
      (dotimes (j cols)
        (setf (aref res j (1- (- rows i))) (aref sa i j))))
    res))

; ch4-2
(defun copy-list-re (lst)
  (reduce #'(lambda (x y) (cons x y)) lst :initial-value '() :from-end t))

(defun reverse-re (lst)
  (reduce #'(lambda (x y) (cons y x)) lst :initial-value '()))

; ch4-3
(defstruct tri-tree
  data
  left
  mid
  right)

(defun copy-tri-tree-r (tree)
  (let ((empty (make-tri-tree)))
    (if (equalp tree empty)
       empty
       (make-tri-tree
          :data (tri-tree-data tree)
          :left (tri-tree-left tree)
          :mid (tri-tree-mid tree)
          :right (tri-tree-right tree)))))

(defun constain-obj (obj tree)
  (cond
   ((null tree) nil)
   ((eql obj (tri-tree-data tree)) t)
   (t (or (constain-obj obj (tri-tree-left tree))
          (constain-obj obj (tri-tree-mid tree))
          (constain-obj obj (tri-tree-right tree))))))

; ch4-4
(defstruct (node (:print-function
                  (lambda (n s d)
                    (format s "#<~A>" (node-elt n)))))
  elt (l nil) (r nil))

(defun tree-to-descending-list (tree)
  (when tree
    (append (tree-to-descending-list (node-r tree))
            (list (node-elt tree))
            (tree-to-descending-list (node-l tree)))))

; ch4-5
(defun bst-adjoin (obj bst <)
  (if (null bst)
      (make-node :elt obj)
      (let ((elt (node-elt bst)))
        (if (eql obj elt)
            bst
            (if (funcall < obj elt)
                (make-node
                   :elt elt
                   :l (bst-adjoin obj (node-l bst) <)
                   :r (node-r bst))
                (make-node
                   :elt elt
                   :r (bst-adjoin obj (node-r bst) <)
                   :l (node-l bst)))))))

; ch4-6
(defun assoc2ht (al)
  (let 
      ((ht (make-hash-table)))
    (dolist (obj al)
      (setf (gethash (car obj) ht) (cdr obj)))
    ht))
(defun ht2assoc (ht)
  (let ((assoc nil))
  (maphash #'(lambda (k v) (push (cons k v) assoc)) ht)
    assoc))