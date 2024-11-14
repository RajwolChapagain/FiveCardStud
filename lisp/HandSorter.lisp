(defun sort-hands (hands)
  (setf hands (sort-by-type hands)))

(defun sort-by-type (hands)
  (sort hands #'hand>))
