(defun main()
  (let ((d0 (read-char))
        (d1 (read-char))
        (d2 (read-char))
        (d3 (read-char)))
    (if (or (and (eq d0 d1) (eq d1 d2)) (and (eq d1 d2) (eq d2 d3)))
      (princ "Yes") (princ "No"))
    (princ #\newline)
    )
)

(main)