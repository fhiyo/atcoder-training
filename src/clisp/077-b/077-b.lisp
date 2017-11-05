(defun maximum (list)
  (reduce #'max list))

(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc))))
    (nreverse acc)))

(defun power-set (ls)
  (if (null ls)
    (list nil)
    (append (power-set (cdr ls))
            (mapcar #'(lambda (xs) (cons (car ls) xs))
                    (power-set (cdr ls))))))

(defun combination (n ls)
  (labels ((comb-sub (n ls)
                     (cond ((zerop n) (list nil))
                           ((= n (length ls)) (list ls))
                           (t
                             (append (mapcar #'(lambda (x) (cons (car ls) x))
                                             (comb-sub (1- n) (cdr ls)))
                                     (comb-sub n (cdr ls)))))))
    (if (< (length ls) n)
      nil
      (comb-sub n ls))))

(defun main()
  (let ((N (read)))
    ; (princ N)
    (loop :for i :from 1 :do
          (if (> (* i i) N) (progn (format t "~D~%" (* (1- i) (1- i))) (return)) nil))
    )
)
(main)
