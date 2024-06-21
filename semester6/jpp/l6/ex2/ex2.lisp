(defun next-row (row)
  (if (null row)
      '(1)
      (let* ((shifted-right (cons 0 row))
             (shifted-left (append row '(0))))
        (mapcar #'+ shifted-right shifted-left))))

(defun iterate (f x n)
  (if (= n 0)
      x
      (iterate f (funcall f x) (- n 1))))

(defun pascal-triangle (n)
  (iterate #'next-row '(1) n))

(defun binomial2 (n k)
  (nth k (pascal-triangle n)))

(format t "~d~%" (binomial2 5 3))
