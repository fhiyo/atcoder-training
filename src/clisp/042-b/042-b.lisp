(defun main()
  (let ((N (read)) (L (read)) (strs nil))
    (loop for str from 0 below N collect
          (setq strs (cons (read) strs)))
    (setq strs (sort strs #'string-lessp))

    (mapcar #'(lambda (x) (format t "~(~a~)" x)) strs)
    (princ '#\newline)))

(main)
