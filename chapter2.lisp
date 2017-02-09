(defparameter *small* 1)
(defparameter *big* 100)

(defvar *foo* 5)
(defvar *foo* 6) ;; not reading because it's definding by defvar

(defun guess-my-number()
  (ash (+ *small* *big*) -1)) ;; ash: binary number managiment


(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun	bigger ()
  (setf *small* (1- (guess-my-number)))
  (guess-my-number))

(defun start-over()
  (setf *small* 1)
  (setf	*big* 100)
  (guess-my-number))


