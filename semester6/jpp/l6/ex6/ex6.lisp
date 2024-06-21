(defun totient (n)
  (labels ((is-coprime (x)
             (= (gcd n x) 1))
           (count-coprimes (x count)
             (if (= x 0)
                 count
                 (count-coprimes (- x 1)
                                 (if (is-coprime x)
                                     (+ count 1)
                                     count)))))
    (count-coprimes n 0)))

(format t "~d~%" (totient 9))
