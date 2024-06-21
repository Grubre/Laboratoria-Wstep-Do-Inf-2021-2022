(defun binomial (n k)
  (cond
    ((= k 0) 1)
    ((= k n) 1)
    ((> k n) 0)
    (t (+ (binomial (- n 1) k) (binomial (- n 1) (- k 1))))))

(print (binomial 5 2))
