; =============== Comparators ===============

(defun compare-royal-flush (h1 h2)
  (> (card-suit (first (get-sorted-cards h1))) (card-suit (first (get-sorted-cards h2)))))


; =============== Main ===============

(defparameter comparators (list #'compare-royal-flush #'compare-royal-flush #'compare-royal-flush #'compare-royal-flush #'compare-royal-flush #'compare-royal-flush #'compare-royal-flush #'compare-royal-flush #'compare-royal-flush #'compare-royal-flush))

(defun sort-hands (hands)
  (setf hands (sort-by-type hands))
  (setf hands (sort-ties hands)))

(defun sort-by-type (hands)
  (sort hands #'hand>))

(defun sort-ties (hands)
  (let ((start-index 0)
        (last-type (hand-type (first hands))))

    (loop for i from 0 to (- (length hands) 1) do
      (cond
        ((/= (hand-type (nth i hands)) last-type) 
         (progn
           (setf (subseq hands start-index i) (sort-subarray (subseq hands start-index i)))
           (setq start-index i)
           (setf last-type (hand-type (nth i hands)))))
        ((= i (- (length hands) 1))
           (setf (subseq hands start-index) (sort-subarray (subseq hands start-index))))))))

(defun sort-subarray (hands)
  (let ((hand-type (hand-type (first hands))))
    (sort hands (nth hand-type comparators))))
