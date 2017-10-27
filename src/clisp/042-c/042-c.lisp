(defun split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
(defun delimiterp (c) (position c " ,.;/"))

(defun solve (dislike_digits_table first_like_digit N_str)
  (loop :for c :across N_str :and idx :from 0 :do
        (loop :while (nth (digit-char-p c) dislike_digits_table)
              :do
                  (cond ((eq c #\9)
                          (if (= idx 0)
                              (return-from solve (solve dislike_digits_table first_like_digit (concatenate 'string "1" (make-string (length N_str) :initial-element #\0))))
                            (return-from solve (solve dislike_digits_table first_like_digit (concatenate 'string (write-to-string (+ (parse-integer (subseq N_str 0 (+ idx 1))) 1)) (make-string (length (subseq N_str (+ idx 1))) :initial-element #\0))
                                   ))))
                        (t
                          (setq c (digit-char (+ (digit-char-p c) 1)))))
                  (setf (char N_str idx) c)
                  (setf (subseq N_str (+ idx 1)) (make-string (length (subseq N_str (+ idx 1))) :initial-element #\0))
              )
        )
  N_str
  )


(defun main()
  (let* ((N (read))
         (K (read))
         (D (read-line))
         (digits (split D))
         (dislike_digits_table (make-list 10))
         (first_like_digit nil)
         )
    (loop :for digit :in digits :collect
          (setf (nth (parse-integer digit) dislike_digits_table) t))
    (setq first_like_digit (position t dislike_digits_table))

    (princ (solve dislike_digits_table first_like_digit (write-to-string N))) (princ '#\newline)))

(main)
