(require 'cl-lib)

(defun problem1-for (lst)
  (cl-loop for i in lst
           sum i))

(defun problem1-while (lst)
  (let ((sum 0)
        (xs lst))
    (while xs
      (setq sum (+ sum (car xs))
            xs (cdr xs)))
    sum))

(defun problem1-recursive (lst)
  (if (null lst)
      0
    (+ (car lst) (problem1-recursive (cdr lst)))))

(defun problem1-tail-recursive (lst)
  (cl-labels ((func (lst acc)
                (if (null lst)
                    acc
                  (func (cdr lst) (+ acc (car lst))))))
    (func lst 0)))

(let ((input '(1 2 3 4 5 6 7 8 9 10)))
  (cl-assert (= 55
                (problem1-for input)
                (problem1-while input)
                (problem1-recursive input)
                (problem1-tail-recursive input))))
