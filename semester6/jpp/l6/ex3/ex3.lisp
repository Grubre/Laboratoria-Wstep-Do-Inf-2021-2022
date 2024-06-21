(defun merge-lists (a b)
  (cond
    ((null a) b)
    ((null b) a)
    ((<= (car a) (car b))
     (cons (car a) (merge-lists (cdr a) b)))
    (t
     (cons (car b) (merge-lists a (cdr b))))))

(defun merge-sort (nums)
  (cond
    ((null nums) '())
    ((null (cdr nums)) nums)
    (t
     (let* ((half (floor (/ (length nums) 2)))
            (left (subseq nums 0 half))
            (right (subseq nums half)))
       (merge-lists (merge-sort left) (merge-sort right))))))

(format t "~a~%" (merge-sort '(3 1 2 4 6 5)))
