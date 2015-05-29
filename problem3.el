(require 'cl-lib)
(require 'calc-arith)

(defun bignum-to-string (n)
  (let ((sign (if (eq (car n) 'bigpos) "" "-")))
    (concat sign
            (mapconcat #'number-to-string
                       (reverse (cdr n))
                       ""))))

(defun problem3 (n)
  (cond ((= n 1) '(0))
        ((= n 2) '(0 1))
        (t
         (cl-loop with acc = '(1 0)
                  repeat (- n 2)
                  do
                  (push (calcFunc-add (cl-first acc) (cl-second acc)) acc)
                  finally
                  return (reverse (mapcar (lambda (e)
                                            (if (listp e) (bignum-to-string e) e)) acc))))))

(cl-assert
 (string= (nth 99 (problem3 100)) "218922995834555169026"))
