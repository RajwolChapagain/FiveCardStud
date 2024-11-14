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

(defun card-from-string (card-string)
  (let* ((value-string (subseq card-string 0 (- (length card-string) 1)))
        (suit-string (subseq card-string (- (length card-string) 1)))
        (value (position value-string *value-map* :test #'string=))
        (suit (position suit-string *suit-map* :test #'string=)))
    (if (or (null value) (null suit))
      (progn
      (format t "ERROR: Invalid string passed for value or suit while initializing card")
      (quit))
    (make-instance 'card :value value :suit suit))))

(defmethod to-string((object card))
  (format nil "~a~a" (nth (card-value object) *value-map*) (nth (card-suit object) *suit-map*)))
