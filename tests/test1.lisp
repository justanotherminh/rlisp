;; Define the factorial function
(defun factorial (n)
  (let ((result 1))
    (do ((i 1 (+ i 1)))
        ((> i n) result)
      (setf result (* result i)))))

;; Define the choose function
(defun choose (n k)
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

;; Calculate choose(12 7)
(let ((result (choose 12 7)))
  (format t "~A~%" result))