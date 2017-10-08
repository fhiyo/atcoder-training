(defun main()
  (let ((l (list (read) (read) (read))))
        (if (equal (sort l #'<) '(5 5 7)) (princ "YES") (princ "NO"))
        (princ '#\newline)))

(main)
