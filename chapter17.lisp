;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; DSL programing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro split (val yes no)
  (let1 g (gensym) ;; g create in read by lisp system
    `(let1 ,g ,val ;; this create in running
       (if ,g
	   (let ((head (car ,g))
		 (tail (cdr ,g)))
	     ,yes)
	   ,no))))

(defun pairs (lst)
  (labels ((f (lst acc)	
	     (split lst	;; lst -> (head tail)
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc)) 
			(reverse acc))
		    (reverse acc))))
    (f lst nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att) ;; get dot-list
	  (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
	alst) ;; alst = ((dot-list) (dot-list) ...)
  (princ #\>))

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
		     (list ,@(mapcar (lambda (x)  ;; making alist
				       `(cons ',(car x) ,(cdr x))) ;; ',(car x) means no calculation
				     (pairs atts))) ;; ((tag . attribute) (tag . attri) ...)
		     nil) ;; no closingp
	  ,@body
	  (print-tag ',name nil t))) ;; close tag

;; (macroexpand '(tag mytag (color 'blur height (+ 4 5))))
;; (PROGN (PRINT-TAG 'MYTAG (LIST (CONS 'COLOR 'BLUR) (CONS 'HEIGHT (+ 4 5))) NIL)
;;	  (PRINT-TAG 'MYTAG NIL T))

;; (print-tag 'mytag (list (cons 'color 'blue) (cons 'height (+ 4 5))) nil
;;	(princ #\<)
;;  	(princ "mytag")
;; 	(format t " ~a=\"~a\"" "color" 'blue)
;;	(format t " ~a=\"~a\"" "height" (+ 4 5))
;;	(princ #\>)

(tag mytag (color 'blue size 'big)
  (tag first_inner_tag ())
  (tag second_inner_tag ()))

;; <mytag color="BLUE" size="BIG">
;;	<first_inner_tag></first_inner_tag>
;;	<second_inner_tag></second_inner_tag>
;;	</mytag>

(tag html ()
  (tag body ()
    (princ "Hello World!")))

;; <html><body>Hello World!</body></html>

(defmacro html (&body body)
  `(tag html ()
     ,@body))

(defmacro body (&body body)
  `(tag body ()
     ,@body))

(html
  (body
    (princ "Hello World!")))

;; <html><body>Hello World!</body></html>

;; svg ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro svg (width height &body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
		   "xmlns:xlink" "http://www.w3.org/1999/xlink" height ,height width ,width)
     ,@body))

(defun brightness (col amt)
  (mapcar (lambda (x)
	    (min 255 (max 0 (+ x amt))))
	  col))

(defun svg-style (color)
  (format nil "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
	  (append color (brightness color -100))))
;; format nil means return result as a text, not as a concole text

(defun circle (center radius color)
  (tag circle (	  cx (car center)
		  cy (cdr center)
		  r radius
		  style (svg-style color))))
(svg 150 150
  (circle `(50 . 50) 50 `(255 0 0))
  (circle `(100 . 100) 50 `(0 0 255)))

;; <svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink" height="150" width="150">
;; 	<circle cx="50" cy="50" r="50" style="fill:rgb(255,0,0);stroke:rgb(155,0,0)">
;;	</circle>
;;	<circle cx="100" cy="100" r="50" style="fill:rgb(0,0,255);stroke:rgb(0,0,155)">
;;	</circle>
;; </svg>

(defun polygon (points color)
  (tag polygon (points (format nil "~{~a,~a ~}" 
			       (mapcan (lambda (tp)
					 (list (car tp) (cdr tp)))
				       points))
		       style (svg-style color))))

;; points="a,b c,d ... "

(defun random-walk (value length)
  (unless (zerop length)
    (cons value
	  (random-walk (if (zerop (random 2))
			   (1- value)
			   (1+ value))
		       (1- length)))))
(random-walk 100 10)

;; (100 99 98 99 98 99 98 97 98 99)

(with-open-file (*standard-output* "random_walk.svg"
				   :direction :output
				   :if-exists :supersede)
  (svg 400 200 (loop repeat 10
		    do (polygon (append '((0 . 200))
					(loop 
					   for x 
					   for y in (random-walk 100 400)
					     collect (cons x y))
					     '((400 . 200)))
				(loop repeat 3
				   collect (random 256))))))

;; Re: wizard house ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "wizards_game.lisp")

;; bucket having chain

(defun have (object)
  (member object (cdr (inventory))))

;; (inventory) -> (items- a b c)

;; (defparameter *chain-welded* nil) ;; chain was connected?

;; (defun weld (subject object)
;;  (if (and (eq *location* 'attic)
;;	   (eq subject 'chain)
;;	   (eq object 'bucket)
;;	   (have 'chain)
;;	   (have 'bucket)
;;	   (not *chain-welded*))
;;      (progn (setf *chain-welded* t)
;;	     '(the chain is now securely welded to the bucket.))
;;      '(you cannot weld like that.)))

;;(pushnew 'weld *allowed-commands*)

;; dunk  bucket

;; (defparameter *bucket-filled* nil)

;;(defun dunk (subject object)
;;  (if (and (eq *location* 'garden)
;;	   (eq subject 'bucket)
;;	   (eq object 'well)
;;	   (have 'bucket)
;;	   *chain-welded*)
;;      (progn (setf *bucket-filled* t)
;;	     '(the bucket is now full of water))
;;      '(you cannot dunk like that.)))

;; (pushnew 'dunk *allowed-commands*)

(defmacro game-action (command subj obj place &body body)
   `(progn (defun ,command (subject object)
	    (if (and (eq *location* ',place)
		     (eq subject ',subj)
		     (eq object ',obj)
		     (have ',subj))
		,@body
		'(i cannot ,command like that.)))
	  (pushnew ',command *allowed-commands*)))

(defparameter *chain-welded* nil)

(game-action weld chain bucket attic
  (if (and (have 'bucket)
	   (not *chain-welded*))
      (progn (setf *chain-welded* t)
	     '(the chain is now securely welded to the bucket))
      '(you do not have a bucket. )))

(defparameter *bucket-filled* nil)

(game-action dunk bucket well garden
  (if *chain-welded*
      (progn (setf *bucket-filled* t)
	     '(the bucket us biw full of water.))
      '(the water level is too low to reach)))

(game-action splash bucket wizard living-room
  (cond ((not *bucket-filled*) '(the bucket has nothing in it.))
	((have 'frog) '(the wizard awakes and sees that you stole his frog.
			he is so upset he banishes you to the netherworlds - you lose! the end.))
	(t '(the wizard awakensfrom his slumber and greets you warmly. 
	     he hands you the magic lo-carb dounut - you win! the end.))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macro make us DLS
;; to help macro, you make sub-function
;;  - it provide us secure programing
;; DSL and functional-programing are friendly each other
;; DSL are useful in some special programing
