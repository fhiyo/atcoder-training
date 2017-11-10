(defun split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
(defun delimiterp (c) (position c " ,.;/"))

(defun make-array-from-list (l)
  (make-array (list (length l)) :initial-contents l)
  )

(defun main()
  (let* ((n (read))
        (ts (make-array-from-list (map 'list #'parse-integer (split (read-line)))))
        (m (read))
        (sum (reduce #'+ ts)))

    (loop :for i :below m :do
          (let ((p (read)) (x (read)))
            (princ (+ (- sum (aref ts (1- p))) x))
            (princ #\newline))))
)
(main)
