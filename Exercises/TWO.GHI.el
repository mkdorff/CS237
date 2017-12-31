
(defun number-of-binary-relations-on (set)
  "Computes the number of binary relations on a set with *n* elements."
  (let ((n (size set))))
  (* n n))
