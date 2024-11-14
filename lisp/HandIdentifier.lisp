(defun is-royal-straight-flush (cards)
  (and (is-royal-straight cards) (is-flush cards)))

(defun is-royal-straight (cards)
  (and (= (card-value (first cards)) 0)
       (= (card-value (second cards)) 9)
       (= (card-value (third cards)) 10)
       (= (card-value (fourth cards)) 11)
       (= (card-value (fifth cards)) 12)))

(defun is-straight-flush (cards)
  (and (is-straight cards) (is-flush cards)))

(defun is-four-of-a-kind (cards)
  (member 4 (get-frequency-set cards)))

(defun is-full-house (cards)
  (and (member 3 (get-frequency-set cards)) (member 2 (get-frequency-set cards))))

(defun is-flush (cards)
  (let ((prev-suit (card-suit (first cards))))
    (dolist (card cards)
      (if (not (= (card-suit card) prev-suit))
        (return-from is-flush nil))))
  T)

(defun is-straight (cards)
  (if (is-royal-straight cards)
    (return-from is-straight T))

  (let ((prev-value (- (card-value (first cards)) 1)))
    (dolist (card cards)
      (if (not (= (card-value card) (+ prev-value 1)))
        (return-from is-straight nil)
        (setq prev-value (card-value card)))))

  (return-from is-straight T))

(defun is-three-of-a-kind (cards)
  (member 3 (get-frequency-set cards)))

(defun is-two-pair (cards)
  (= (count 2 (get-frequency-set cards)) 2))

(defun assign-type (hand)
  (let ((cards (get-sorted-cards hand)))
    (cond 
      ((is-royal-straight-flush cards) (setf (hand-type hand) 9))
      ((is-straight-flush cards) (setf (hand-type hand) 8))
      ((is-four-of-a-kind cards) (setf (hand-type hand) 7))
      ((is-full-house cards) (setf (hand-type hand) 6))
      ((is-flush cards) (setf (hand-type hand) 5))
      ((is-straight cards) (setf (hand-type hand) 4))
      ((is-three-of-a-kind cards) (setf (hand-type hand) 3))
      ((is-two-pair cards) (setf (hand-type hand) 2))
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
