(defun factorial (n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(defun factorial_loop (n)
  (let ((result 1))
    (do ((i 1 (+ i 1)))
        ((> i n) result)
      (setf result (* result i)))))

(defun choose (n k)
  (/ (factorial_loop n)
     (* (factorial_loop k) (factorial_loop (- n k)))))

(let ((result (choose 12 7)))
  (write result))

(let ((assert_eq (= (factorial 11) (factorial_loop 11))))
  (write assert_eq))