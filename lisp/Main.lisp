; =============== Non-testing functions ===============
(defun create-deck ()
  (loop for i from 0 to 51
        collect (make-instance 'card :value (mod i 13) :suit (floor i 13))))

(defun shuffle-deck(deck)
  (setq *random-state* (make-random-state t))
  (let ((random-int)
        (temp))
    (loop for i from 0 to 51 do
          (setq random-int (random 52))
          (setf temp (nth i deck))
          (setf (nth i deck) (nth random-int deck))
          (setf (nth random-int deck) temp))))

(defun print-deck(deck)
  (format t "*** USING RANDOMIZED DECK OF CARDS ***~%~%")                                
  (format t "*** Shuffled 52 card deck:~%")
    (dotimes (index (length deck))
      (format t "~4a" (to-string (nth index deck)))
      (if (= (mod (+ index 1) 13) 0)
        (terpri))))

; =============== Logic ===============
(format t "*** P O K E R   H A N D   A N A L Y Z E R ***~%~%~%")

(let ((deck (create-deck)))
  (shuffle-deck deck)
  (print-deck deck))
