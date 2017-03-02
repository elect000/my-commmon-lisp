;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; web server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(sin 0.5) ;; 0.4792555

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; how to make program ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; beautiful ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-widget (database widget)
  (setf database (remove 'quit database))
  (cons widget database))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dirty ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; non-function
(defparameter *database* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main-loop ()
  (loop 
     do (progn (princ "Please enter the name of a new widget: ")
	       (setf *database* (add-widget *database* (read)))
	       (unless (find 'quit *database*)
		 (format t "The database contains the following: ~a~%" *database*)))
     until (equal (car *database*) 'quit)))

;; non-functional-style ;;;;;;;;;;;;;;;;;

(defparameter *my-list* '(4 7 2 3))

(loop for n below (length *my-list*)
     do (setf (nth n *my-list*) (+ (nth n *my-list*) 2)))

;; functional-style ;;;;;;;;;;;;;;;;;;;;;;

(defparameter *my-list* '(4 7 2 3))

(defun add-two (list)
  (when list
    (cons (+ 2 (car list)) (add-two (cdr list)))))

;; high-layer-programing ;;;;;;;;;;;;;;;;;

(mapcar (lambda (x)
	  (+ x 2))
	'(4 6 2 3))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; - functional-style has same result with same argument
;; - functional-program has not sub-action such as change data
;; - order-style-programing is non-functional-program
;; - lisp programing has two fields : dirty region / beautiful region
