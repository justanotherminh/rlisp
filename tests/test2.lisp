(defstruct node
  value
  next)

(defstruct stack
  top)

(defun push (stack val)
  (let ((new-node (make-node :value val :next (stack-top stack))))
    (setf (stack-top stack) new-node)))

(defun pop (stack)
  (when (not (stack-empty stack))
    (let ((top-val (node-value (stack-top stack))))
      (setf (stack-top stack) (node-next (stack-top stack)))
      top-val)))

(defun peek (stack)
  (when (not (stack-empty stack))
    (node-value (stack-top stack))))

(defun stack-empty (stack)
  (not (stack-top stack)))

(defun reverse-list (lst)
  (let ((s (make-stack)))
    (dolist (item lst)
      (push s item))
    (let ((result '()))
      (loop until (stack-empty s) 
            do (push result (pop s)))
      result)))

(reverse-list '(1 2 3 4 5))  ; => (5 4 3 2 1)

(reverse-list '("apple" "banana" "cherry"))  ; => ("cherry" "banana" "apple")