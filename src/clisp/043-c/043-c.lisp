(defun split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
(defun delimiterp (c) (position c " ,.;/"))

(defun main ()
  (let ((n (read))
        (a_list (map 'list #'parse-integer (split (read-line))))
        (x nil))
    (setq x (/ (reduce #'+ a_list) n))
    (if (>= (- x (floor x)) (- (ceiling x) x))
      (setq x (ceiling x))
      (setq x (floor x)))

    (princ
      (reduce
        #'+
        (map 'list #'(lambda (l) (* (- l x) (- l x))) a_list)))
    (princ #\newline)

    ))
(main)
