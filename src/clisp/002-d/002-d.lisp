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

(defun main ()
  (let ((n (read)) (m (read))
        (acq nil))
    (loop :repeat m :do
          (setq acq (cons (list (read) (read)) acq)))
    ; (print acq)
    (princ (maximum (mapcar #'length (remove-if-not
                               #'(lambda (mem)
                                   (every
                                     #'(lambda (rel)
                                         (member rel acq :test #'equal)) (combination 2 mem)))
                               (power-set (loop :for i :from 1 :upto n :collect i))))))
    (princ #\newline)
    ))
(main)
