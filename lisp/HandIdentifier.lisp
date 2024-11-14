(defun is-royal-straight-flush (cards)
  (and (is-royal-straight cards) (is-flush cards)))

(defun is-royal-straight (cards)
  (and (= (card-value (first cards)) 0)
       (= (card-value (second cards)) 9)
       (= (card-value (third cards)) 10)
       (= (card-value (fourth cards)) 11)
       (= (card-value (fifth cards)) 12)))

(defun is-flush (cards)
  (let ((prev-suit (card-suit (first cards))))
    (dolist (card cards)
      (if (not (= (card-suit card) prev-suit))
        (return-from is-flush nil))))
  T)

(defun assign-type (hand)
  (let ((cards (get-sorted-cards hand)))
    (cond 
      ((is-royal-straight-flush cards) (setf (hand-type hand) 9))
      ((is-flush cards) (setf (hand-type hand) 5))
      ((setf (hand-type hand) 0)))))

; =============== Helpers ===============

(defun get-frequency-set (cards)
  (let ((frequency-set (list))
        (prev-value -1))
    (dolist (card cards)
      (if (not (= prev-value (card-value card)))
        (progn
          (push (count card cards :test #'card=) frequency-set)
          (setq prev-value (card-value card)))))
    (sort frequency-set #'<)))
