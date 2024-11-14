(load "Card.lisp")
(load "Hand.lisp")
(load "HandIdentifier.lisp")
(load "HandSorter.lisp")

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
        (terpri)))
    (terpri))

(defun deal-from-deck(hands deck)
  (dotimes (i *hand-size*)
    (dolist (hand hands)
      (add-card hand (pop deck))))
  deck)

(defun print-remaining-deck(deck)
  (format t "*** Here is what remains in the deck...~%")
  (dolist (card deck)
    (format t "~4a" (to-string card)))
    (terpri)
    (terpri))

; =============== Testing functions ===============
(defun print-file(path)
  (format t "*** USING TEST DECK ***~%~%")

  (format t "*** File: ~a~%" path)
  (with-open-file (stream path :direction :input)
    (loop for line = (read-line stream nil)
          while line
          do (format t "~a~%" line)))
  (terpri))

(defun deal-from-file (hands path)
  (with-open-file (stream path :direction :input)
    (let ((i 0))
      (loop for line = (read-line stream nil)
            while line
            do (progn 
                 (setf (nth i hands) (hand-from-string line))
                 (incf i))))))

(defun check-duplicate (hands)
  (let ((hashes (list)))
    (dolist (hand hands)
      (dolist (card (get-sorted-cards hand))
        (let ((card-hash (+ (* (card-value card) 10) (card-suit card))))
          (if (member card-hash hashes)
            (progn
              (format t "*** ERROR - DUPLICATED CARD FOUND IN DECK ***~%~%")
              (format t "*** Duplicate: ~a ***~%" (to-string card))
              (quit))
            (push card-hash hashes)))))))

; =============== Common functions ===============
(defun print-hands (hands)
  (format t "*** Here are the six hands...~%")
  (dolist (hand hands)
    (format t "~a~%" (to-string hand)))
  (terpri))

(defun assign-types (hands)
  (dolist (hand hands)
    (assign-type hand)))

(defun print-ranked-hands (hands)
  (format t "--- WINNING HAND ORDER ---~%")

  (dolist (hand hands)
    (format t "~a - ~a~%" (to-string hand) (nth (hand-type hand) *hand-map*))))

; =============== Logic ===============
(format t "*** P O K E R   H A N D   A N A L Y Z E R ***~%~%~%")
(defvar hands (list (make-instance 'hand) (make-instance 'hand) (make-instance 'hand) (make-instance 'hand) (make-instance 'hand) (make-instance 'hand)))

(cond 
  ((= (length sb-ext:*posix-argv*) 1)
    (let ((deck (create-deck)))
      (shuffle-deck deck)
      (print-deck deck)
      (setf deck (deal-from-deck hands deck))
      (print-hands hands)
      (print-remaining-deck deck))
    (assign-types hands)
    (sort-hands hands)
    (print-ranked-hands hands))
  ((= (length sb-ext:*posix-argv*) 2)
   (let ((filepath (second sb-ext:*posix-argv*)))
     (print-file filepath)
     (deal-from-file hands filepath))
   (check-duplicate hands)
   (print-hands hands)
   (assign-types hands)
   (sort-hands hands)
   (print-ranked-hands hands)))
