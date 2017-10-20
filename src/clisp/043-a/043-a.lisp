(defun sum (n)
  (if (= n 0) 0
    (+ (sum (- n 1)) n)))

(defun main ()
  (let ((n (read)))
        (princ (sum n)) (princ #\newline)))

(main)
