(require 'cl-lib)

(defun concat-num (a b)
  (string-to-number (concat (number-to-string a) (number-to-string b))))

(defun permute-ops (n)
  (let ((accs nil))
    (cl-labels ((permute-ops- (n i acc)
                  (if (= n i)
                      (push (reverse acc) accs)
                    (cl-loop for op in '(+ - concat-num)
                             do
                             (permute-ops- n (1+ i) (cons op acc))))))
      (permute-ops- n 0 '())
      accs)))

(defun apply-concat (lst ops)
  (cl-loop with stack = (list (car lst))
           for e in (cdr lst)
           for op in ops
           if (eq op 'concat-num)
           do
           (let ((a (pop stack)))
             (push (concat-num a e) stack))
           else
           do
           (push e stack)
           finally return (reverse stack)))

(defun calc-ops (lst- ops-)
  (let ((lst (apply-concat lst- ops-))
        (ops (cl-loop for op in ops-
                      unless (eq op 'concat-num)
                      collect op)))
    (if (null ops) ;; all ops are concat
        (car lst)
      (cl-loop with acc = (funcall (car ops) (cl-first lst) (cl-second lst))
               for op in (cdr ops)
               for num in (cddr lst)
               do
               (setq acc (funcall op acc num))
               finally return acc))))

(defun format-ops (lst ops)
  (concat (number-to-string (car lst))
          (cl-loop for op in ops
                   for num in (cdr lst)
                   if (eq op 'concat-num)
                   concat (number-to-string num)
                   else
                   concat (format "%s%s" op num))))

(defun problem5 (lst)
  (let ((opss (permute-ops (1- (length lst)))))
    (cl-loop for ops in opss
             when (= (calc-ops lst ops) 100)
             collect (format-ops lst ops))))

(cl-assert
 (member "1+2+34-5+67-8+9" (problem5 '(1 2 3 4 5 6 7 8 9))))
