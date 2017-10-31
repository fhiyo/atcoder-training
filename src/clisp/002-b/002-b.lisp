(defun main ()
  (let ((w (read-line)))
    (loop :for c :across w :do
          (if (not (find c "aiueo")) (princ c) nil))
    (princ #\newline)))

(main)
