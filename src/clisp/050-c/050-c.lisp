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

(defun modExpt (a e m)
  (let ((res 1) (bit_str (format nil "~B" e)))
    (loop :for c :across bit_str :do
          (if (eq c #\0)
            (setq res (mod (* res res) m))
            (setq res (mod (* (mod (* res res) m) a) m))
            )
          )
    res))

(defun main()
  (let ((mod_ 1000000007)
        (n (read))
        (as (sort (make-array-from-list (map 'list #'parse-integer (split (read-line)))) #'<))
         (checker 1))

    (cond ((oddp (mod n 2)) (if (not (= (aref as 0) 0))
                              (progn (princ 0) (princ #\newline) (return-from main)))
                            (setq as (subseq as 1 (length as))) (setq checker 2))
          )

    (loop :for a :across as :and i :from 0 :do
          (cond ((evenp (mod i 2)) (if (not (= a checker))
                                     (progn (princ 0) (princ #\newline) (return-from main))))
                (t (if (not (= a checker))
                     (progn (princ 0) (princ #\newline) (return-from main)) (setq checker (+ checker 2))))))

    (princ (modExpt 2 (floor (/ n 2)) mod_)) (princ #\newline)
    )
)
(main)
