(defparameter *mod* 1000000007)

(defun modExpt (a e m)
  (let ((res 1) (bit_str (format nil "~B" e)))
    (loop :for c :across bit_str :do
          (if (eq c #\0)
            (setq res (mod (* res res) m))
            (setq res (mod (* (mod (* res res) m) a) m))
            )
          )
    res))

(defun main ()
  (let* ((h (read)) (w (read)) (a (read)) (b (read))
         (fact_table (make-array (- (+ h w) 1) :initial-element 1))
         (inv_fact_table (make-array (- (+ h w) 1) :initial-element 1))
         (result 0))
    (loop :for i :from 2 :below (- (+ h w) 1) :do
          (setf (aref fact_table i) (mod (* (aref fact_table (- i 1)) i) *mod*)))

    (loop :for i :below (- (+ h w) 1) :do
          (setf (aref inv_fact_table i) (modExpt (aref fact_table i) (- *mod* 2) *mod*)))

    (defun comb (n r m)
      (mod (* (mod (* (aref fact_table n) (aref inv_fact_table r)) m) (aref inv_fact_table (- n r))) m))

    (loop :for i :from b :upto (- w 1) :do
          (setq result (+ result
                          (* (comb (- (+ (- h a) i) 1) (- (- h a) 1) *mod*)
                             (comb (- (- (+ w a) i) 2) (- a 1) *mod*))))
          )
    (setq result (mod result *mod*))
    (princ result) (princ #\newline)
    ))

(main)
