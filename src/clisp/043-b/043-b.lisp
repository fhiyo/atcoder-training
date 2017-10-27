(defun make-adjustable-string (s)
  (make-array (length s)
              :fill-pointer (length s)
              :adjustable t
              :initial-contents s
              :element-type (array-element-type s)))

(defun main ()
  (let ((s (read-line)) (res (make-adjustable-string "")))
    (loop :for c :across s :do
          (if (eq c #\B)
            (if (= (length res) 0) 'nil (vector-pop res))
            (vector-push-extend c res)))
    (princ res) (princ #\newline)))

(main)
