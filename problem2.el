(require 'cl-lib)

(defun problem2 (xs ys)
  (cl-loop for x in xs
           for y in ys
           collect x
           collect y))

(cl-assert (equal '(a 1 b 2 c 3)
                  (problem2 '(a b c) '(1 2 3))))
