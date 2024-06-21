(defun prime-factors (n)
  (labels ((factor (n d)
             (cond
               ((< n 2) nil)
               ((< n (* d d)) (list n))
               ((zerop (mod n d))
                (cons d (factor (floor (/ n d)) d)))
               (t (factor n (+ d 1))))))
    (factor n 2)))

(defun totient2 (n)
  (cond
    ((<= n 1) 0)
    (t
     (let ((f nil))
       (setq f (lambda (lst)
                 (cond
                   ((null (cdr lst)) (- (car lst) 1))
                   ((= (car lst) (cadr lst)) (* (car lst) (funcall f (cdr lst))))
                   (t (* (- (car lst) 1) (funcall f (cdr lst)))))))
       (funcall f (prime-factors n))))))

(format t "~D~%" (totient2 9))
