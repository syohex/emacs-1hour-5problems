(require 'cl-lib)

(defun problem3 (n)
  (cond ((= n 1) '(0))
        ((= n 2) '(0 1))
        (t
         (cl-loop with acc = '(1 0)
                  repeat (- n 2)
                  do
                  (push (+ (cl-first acc) (cl-second acc)) acc)
                  finally
                  return (reverse acc)))))

(problem3 100)
