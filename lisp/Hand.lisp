(defparameter *hand-map* (list "High Card" "Pair" "Two Pair" "Three of a Kind" "Straight" "Flush" "Full House" "Four of a Kind" "Straight Flush" "Royal Straight Flush"))

(defgeneric add-card(hand card))

(defclass hand()
  ((cards :accessor hand-cards
          :initform ()
          :type list)
   (hand-type :accessor hand-type
              :initarg :hand-type
              :type integer)))

(defmethod add-card(hand card)
    (setf (hand-cards hand) (nconc (hand-cards hand) (list card))))
