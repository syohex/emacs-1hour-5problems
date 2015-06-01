(require 'cl-lib)

(defun compare (a b)
  (let ((ab-val (string-to-number (format "%s%s" a b)))
        (ba-val (string-to-number (format "%s%s" b a))))
    (> ab-val ba-val)))

(defun problem4 (lst)
  (let ((xs (sort lst #'compare)))
    (string-to-number (mapconcat #'number-to-string xs ""))))

(cl-assert
 (= (problem4 '(420 42 423)) 42423420)
 (= (problem4 '(50 2 1 9)) 95021))
