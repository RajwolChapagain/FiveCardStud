; =============== Comparators ===============
; All return:
; T if h1 > h2
; NIL if h1 < h2

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

; =============== Helpers ===============

; Returns:
;  1 if l1 < l2
; -1 if l1 > l2
;  0 if l1 = l2
(defun compare-highest-card (l1 l2)
  (let ((value-list1 (list))
        (value-list2 (list)))

    (loop for i from 0 to (- (length l1) 1) do
          (progn
            (setf value-list1 (nconc value-list1 (list (card-value (nth i l1)))))
            (if (= (card-value (nth i l1)) 0)
              (progn
                (setf (nth i value-list1) 13)
                (if (= (length l1) *hand-size*)
                  (progn
                    (if (and (is-straight l1) (not (is-royal-straight l1)))
                      (setf (nth i value-list1) 0))))))

            (setf value-list2 (nconc value-list2 (list (card-value (nth i l2)))))
            (if (= (card-value (nth i l2)) 0)
              (progn
                (setf (nth i value-list2) 13)
                (if (= (length l2) *hand-size*)
                  (progn
                    (if (and (is-straight l2) (not (is-royal-straight l2)))
                      (setf (nth i value-list2) 0))))))))

    (setf value-list1 (sort value-list1 #'<))
    (setf value-list2 (sort value-list2 #'<))

    (loop for i from 0 to (- (length value-list1) 1) do
          (cond
            ((< (nth i value-list1) (nth i value-list2)) (return-from compare-highest-card 1))
            ((> (nth i value-list1) (nth i value-list2)) (return-from compare-highest-card -1))))

    (return-from compare-highest-card 0)))

(defun get-cards-occuring-n-times (card-list n )
  (let ((result (list)))

    (dolist (card card-list)
      (if (not (member card result :test #'card=))
        (if (= (count card card-list :test #'card=) n)
          (push card result))))

    (sort result #'card<)))
