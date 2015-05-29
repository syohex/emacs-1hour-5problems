(require 'cl-lib)

(defun problem4 (lst)
  (let ((xs (sort lst (lambda (a b)
                        (string< (number-to-string b) (number-to-string a))))))
    (string-to-number (mapconcat #'number-to-string xs ""))))

(cl-assert
 (= (problem4 '(50 2 1 9)) 95021))
