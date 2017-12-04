(defun lucas (n)
  (let ((memo '(1 2)))
    (loop :for i :from 2 :upto n :do
          (setq memo (cons (+ (car memo) (cadr memo)) memo)))
    (if (= n 0) (cadr memo) (car memo))
    ))

(defun main()
  (let ((n (read)))
    (princ (lucas n))
    (princ #\newline)
    )
)
(main)
