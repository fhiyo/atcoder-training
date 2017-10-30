(defun solve (s)
  (cond ((< (length s) 2) (format t "~D ~D~%" -1 -1) (return-from solve))
        ((= (length s) 2) (if (eq (aref s 0) (aref s 1))
                            (progn (format t "~D ~D~%" 1 2) (return-from solve))
                            (progn (format t "~D ~D~%" -1 -1) (return-from solve)))
                          )
        (t
          (loop :for c :across s :and i :from 0 :below (- (length s) 2) :do
                (cond ((eq (aref s i) (aref s (+ i 1)))
                       (format t "~D ~D~%" (+ i 1) (+ i 2))
                       (return-from solve))
                      ((eq (aref s i) (aref s (+ i 2)))
                       (format t "~D ~D~%" (+ i 1) (+ i 3))
                       (return-from solve))
                      ))))
  (format t "~D ~D~%" -1 -1))

(defun main()
  (let ((s (read-line)))
    (solve s)
    ))

(main)
