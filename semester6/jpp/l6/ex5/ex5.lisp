(defun prime-factors (n)
  (labels ((factor (n d)
             (cond
               ((< n 2) (list))
               ((< n (* d d)) (list n))
               ((zerop (mod n d))
                (cons d (factor (floor (/ n d)) d)))
               (t (factor n (+ d 1))))))
    (factor n 2)))

(format t "~{~a~^, ~}" (prime-factors 315))
