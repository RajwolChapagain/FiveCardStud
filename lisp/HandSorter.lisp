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

; =============== Comparators ===============
; All return:
; T if h1 > h2
; NIL if h1 < h2

(defun compare-royal-flush (h1 h2)
  (> (card-suit (first (get-sorted-cards h1))) (card-suit (first (get-sorted-cards h2)))))

(defun compare-straight-flush (h1 h2)
  (let* ((l1 (get-sorted-cards h1))
        (l2 (get-sorted-cards h2))
        (highest-card-comparison (compare-highest-card l1 l2)))

    (if (= highest-card-comparison 1)
      (return-from compare-straight-flush NIL))
    (if (= highest-card-comparison -1)
      (return-from compare-straight-flush T))

    (> (card-suit (first l1)) (card-suit (first l2)))))

(defun compare-four-of-a-kind (h1 h2)
  (let* ((l1 (get-sorted-cards h1))
        (l2 (get-sorted-cards h2))
        (c1 (first (get-cards-occuring-n-times l1 4)))
        (c2 (first (get-cards-occuring-n-times l2 4)))
        (value1 (card-value c1))
        (value2 (card-value c2)))

    (if (= value1 0) (setq value1 13))
    (if (= value2 0) (setq value2 13))

    (> value1 value2)))

(defun compare-full-house (h1 h2)
  (let* ((l1 (get-sorted-cards h1))
        (l2 (get-sorted-cards h2))
        (c1 (first (get-cards-occuring-n-times l1 3)))
        (c2 (first (get-cards-occuring-n-times l2 3)))
        (value1 (card-value c1))
        (value2 (card-value c2)))

    (if (= value1 0) (setq value1 13))
    (if (= value2 0) (setq value2 13))

    (> value1 value2)))

(defun compare-flush (h1 h2)
  (let* ((l1 (get-sorted-cards h1))
        (l2 (get-sorted-cards h2))
        (highest-card-comparison (compare-highest-card l1 l2)))

    (if (= highest-card-comparison 1)
      (return-from compare-flush NIL))
    (if (= highest-card-comparison -1)
      (return-from compare-flush T))

    (> (card-suit (first l1)) (card-suit (first l2)))))

(defun compare-straight (h1 h2)
  (let* ((l1 (get-sorted-cards h1))
        (l2 (get-sorted-cards h2))
        (highest-card-comparison (compare-highest-card l1 l2)))

    (if (= highest-card-comparison 1)
      (return-from compare-straight NIL))
    (if (= highest-card-comparison -1)
      (return-from compare-straight T))

    (let ((highest-card-suit1 (card-suit (nth (- (length l1) 1) l1)))
          (highest-card-suit2 (card-suit (nth (- (length l2) 1) l2))))

          (if (is-royal-straight l1)
            (setf highest-card-suit1 (card-suit (first l1))))
          (if (is-royal-straight l2)
            (setf highest-card-suit2 (card-suit (first l2))))

          (> highest-card-suit1 highest-card-suit2))))

(defun compare-three-of-a-kind (h1 h2)
  (let* ((l1 (get-sorted-cards h1))
        (l2 (get-sorted-cards h2))
        (c1 (first (get-cards-occuring-n-times l1 3)))
        (c2 (first (get-cards-occuring-n-times l2 3)))
        (value1 (card-value c1))
        (value2 (card-value c2)))

    (if (= value1 0) (setq value1 13))
    (if (= value2 0) (setq value2 13))

    (> value1 value2)))

; =============== Main ===============

(defparameter comparators (list #'compare-royal-flush #'compare-royal-flush #'compare-royal-flush #'compare-three-of-a-kind #'compare-straight #'compare-flush #'compare-full-house #'compare-four-of-a-kind #'compare-straight-flush #'compare-royal-flush))

(defun sort-hands (hands)
  (setf hands (sort-by-type hands))
  (sort-ties hands)
  hands)

(defun sort-by-type (hands)
  (sort (copy-list hands) #'hand>))

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
