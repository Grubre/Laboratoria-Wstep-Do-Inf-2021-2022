(defun gen_nums (start end)
  (labels ((generate-sequence (current end result)
             (if (> current end)
                 result
                 (generate-sequence (1+ current) end (cons current result)))))
    (reverse (generate-sequence start end '()))))

(defun primes (n)
  (if (< n 2)
      nil
      (labels ((erathosthenes (lst)
                 (cond ((null lst) nil)
                       ((> (* (car lst) (car lst)) n) lst)
                       (t (cons (car lst)
                                (erathosthenes (remove-if (lambda (x) (zerop (mod x (car lst))))
                                                          (cdr lst))))))))
        (erathosthenes (gen_nums 2 n)))))

(format t "~{~a~^, ~}" (primes 100))
