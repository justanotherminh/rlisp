(defun factorial (x)
   (if (zerop x)
       1
       (* x (factorial (- x 1)))))