(defparameter *value-map* (list "A" "2" "3" "4" "5" "6" "7" "8" "9" "10" "J" "Q" "K"))
(defparameter *suit-map* (list "D" "C" "H" "S"))

(defgeneric to-string(object))

(defclass card()
  ((value :accessor card-value
          :initarg :value
          :type integer)
   (suit :accessor card-suit
         :initarg :suit
         :type integer)))

(defmethod to-string((object card))
  (format nil "~a~a" (nth (card-value object) *value-map*) (nth (card-suit object) *suit-map*)))
