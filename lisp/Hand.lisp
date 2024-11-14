(defparameter *hand-map* (list "High Card" "Pair" "Two Pair" "Three of a Kind" "Straight" "Flush" "Full House" "Four of a Kind" "Straight Flush" "Royal Straight Flush"))

(defgeneric add-card(hand card))
(defgeneric to-string(hand))
(defgeneric get-sorted-cards(hand))
(defgeneric hand>(hand1 hand2))

(defclass hand()
  ((cards :accessor hand-cards
          :initform ()
          :type list)
   (hand-type :accessor hand-type
              :initarg :hand-type
              :type integer)))

(defun comma-split (string)
  (loop for start = 0 then (1+ finish)
        for finish = (position #\, string :start start)
        collecting (subseq string start finish)
        until (null finish)))

(defun hand-from-string(line)
  (let ((hand (make-instance 'hand)))
    (dolist (token (comma-split line))
      (let ((card (card-from-string token)))
        (add-card hand card)))
    hand))


(defmethod add-card(hand card)
    (setf (hand-cards hand) (nconc (hand-cards hand) (list card))))

(defmethod to-string(hand)
  (let ((result ""))
    (dolist (card (hand-cards hand) result)
      (setq result (concatenate 'string result (format nil "~4a" (to-string card)))))))

(defmethod get-sorted-cards(hand)
    (sort (copy-list (hand-cards hand)) #'card<))

(defmethod hand>(hand1 hand2)
  (> (hand-type hand1) (hand-type hand2)))
