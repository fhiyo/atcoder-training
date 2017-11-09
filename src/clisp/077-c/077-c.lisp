(defun split (string &key (delimiterp #'delimiterp))
  (loop :for beg = (position-if-not delimiterp string)
    :then (position-if-not delimiterp string :start (1+ end))
    :for end = (and beg (position-if delimiterp string :start beg))
    :when beg :collect (subseq string beg end)
    :while end))
(defun delimiterp (c) (position c " ,.;/"))

; (defun maximum (list)
;   (reduce #'max list))
;
; (defun filter (fn lst)
;   (let ((acc nil))
;     (dolist (x lst)
;       (let ((val (funcall fn x)))
;         (if val (push val acc))))
;     (nreverse acc)))
;
; (defun power-set (ls)
;   (if (null ls)
;     (list nil)
;     (append (power-set (cdr ls))
;             (mapcar #'(lambda (xs) (cons (car ls) xs))
;                     (power-set (cdr ls))))))
;
; (defun combination (n ls)
;   (labels ((comb-sub (n ls)
;                      (cond ((zerop n) (list nil))
;                            ((= n (length ls)) (list ls))
;                            (t
;                              (append (mapcar #'(lambda (x) (cons (car ls) x))
;                                              (comb-sub (1- n) (cdr ls)))
;                                      (comb-sub n (cdr ls)))))))
;     (if (< (length ls) n)
;       nil
;       (comb-sub n ls))))

(defun make-array-from-list (l)
  (make-array (list (length l)) :initial-contents l)
  )

(defun bisect-left (n arr)
  (bisect-left_ n 0 (length arr) arr))
(defun bisect-left_ (n lo hi arr)
  (cond ((>= lo hi) (return-from bisect-left_ lo))
        (t
          (let ((mid (floor (/ (+ lo hi) 2))))
            (cond ((<= n (aref arr mid)) (return-from bisect-left_
                                                      (bisect-left_ n lo mid arr)))
                  (t (return-from bisect-left_ (bisect-left_ n (1+ mid) hi arr))))))
        ))

(defun bisect-right (n arr)
  (bisect-right_ n 0 (length arr) arr))
(defun bisect-right_ (n lo hi arr)
  (cond ((>= lo hi) (return-from bisect-right_ hi))
        (t
          (let ((mid (floor (/ (+ lo hi) 2))))
            (cond ((< n (aref arr mid)) (return-from bisect-right_
                                                      (bisect-right_ n lo mid arr)))
                  (t (return-from bisect-right_ (bisect-right_ n (1+ mid) hi arr))))))
        ))

(defun main()
  (let ((n (read))
        (as (sort (make-array-from-list (map 'list #'parse-integer (split (read-line)))) #'<))
        (bs (map 'list #'parse-integer (split (read-line))))
        (cs (sort (make-array-from-list (map 'list #'parse-integer (split (read-line)))) #'<))
        )

    (princ (reduce #'+ (loop :for b :in bs :collect
                       (* (bisect-left b as) (- n (bisect-right b cs))))))
    (princ #\newline)
    )
)
(main)

    ; (loop :for b :in bs :collect
    ;       (* (bisect-left b as) (- n (bisect-right b cs))))
    ; (loop :for b :in bs :do
    ;       (format t "~D ~D~%" (bisect-left b as) (- n (bisect-right b cs)))
    ;       )
    ; (print as)
    ; (let ((sample (read)))
    ;   (print (bisect-left sample as))
    ;   (print (bisect-right sample as))
    ;   )

