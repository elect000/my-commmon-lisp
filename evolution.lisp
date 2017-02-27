;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; loop/format ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluating animals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 40)

(defparameter *plants* (make-hash-table :test #'equal)) ;; usually use #'eql
;; review eql -> symbol number character 
;; review equal -> neally all

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t))) ;; we don't think the value, so we only use "t"
;; review random A -> 0 <= return <= A-1

(defun add-plants ()		  ;; it create two plants
  (apply #'random-plant *jungle*) ;; *jungle* = '(A B C D) ... number's list
  (random-plant 0 0 *width* *height*))


;; abstract structure
(defstruct animal x y energy dir genes)  ;; have 5 elements

(defparameter *animals* 		   ;; this is a list, so it is first animal
  (list (make-animal :x (ash *width* -1)   ;; half of map
		     :y (ash *height* -1)  ;; half of map
		     :energy 100	   ;; life
		     :dir 0		   ;; initial direction
		     :genes (loop repeat 8 ;; this value is power/ not direction
				 collecting (1+ (random 10)))))) ;; element of genes: 1 <= x <= 10
;;;;;;;;;;;
;;  dir  ;;
;; 0 1 2 ;;
;; 7   3 ;;
;; 6 5 4 ;;
;;;;;;;;;;;

(defun move (animal)
  (let ((dir (animal-dir animal))
	(x (animal-x animal))
	(y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
				    (cond ((and (>= dir 2) (< dir 5)) 1) ;; 2<= dir < 5 
					  ((or (= dir 1) (= dir 5)) 0)   ;; dir = 1 or 5
					  (t -1)))
				 *width*)) ;; mod a b .. a < b/ if move max -> move to min 
    (setf (animal-y animal) (mod (+ y
				    (cond ((and (>= dir 0) (< dir 3)) -1)
					  ((and (= dir 7) (= dir 3)) 0)
					   (t 1)))
				 *height*))
    (decf (animal-energy animal)))) ;; mod a b

(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal))))) ;; 0 <= x < sum '(animal-genes)
    (labels ((angle (genes x)				;; genes x
	       (let ((xnu (- x (car genes))))		;; loop start
		 (if (< xnu 0)
		     0					;; it has no meaning == damp data
		     (1+ (angle (cdr genes) xnu))))))	;; loop end
      (setf (animal-dir animal)				;; set direction
	    (mod (+ (animal-dir animal) (angle (animal-genes animal) x)) ;; add now state and xnu
		 8)))))					;; 0 <= dir < 7

(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*) ;; there is a plant
      (incf (animal-energy animal) *plant-energy*) 
      (remhash pos *plants*))))

;; remhash == remove hash
;; review :: incf A B ... A = A + B

(defparameter *reproduction-energy* 400)

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -2)) ;; half of latest energy
      (let ((animal-nu 	(copy-structure animal))
	    (genes 	(copy-list (animal-genes animal)))
	    (mutation 	(random 8)))  ;; which slot of the gene list will change ?
	(setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1))) 
	(setf (animal-genes animal-nu) genes) ;; re-defun genes
	(push animal-nu *animals*)))))	      ;; add the list of animals

;; (nth mutation genes) + (-1 <= x <= 1) or 1 (minimam = 1)

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
			       (<= (animal-energy animal) 0)) ;; killed enegy
			     *animals*))
  (mapc (lambda (animal) ;; mapc will not make list
	  (turn animal)
	  (move animal) ;; always 1 step move
	  (eat animal)
	  (reproduce animal))
	*animals*)
  (add-plants))

(defun draw-world ()
  (loop for y
       below *height*
       do (progn (fresh-line)
		 (princ "|")
		 (loop for x
		      below *width* ;; 0 <= animal, plant <= *width* - 1 
		      do (princ (cond ((some (lambda (animal)
					       (and 	(= (animal-x animal) x)
					       		(= (animal-y animal) y)))
					     *animals*)
				       #\M)
				      ((gethash (cons x y) *plants*)
				       #\*)
				      (t #\space))))
		 (princ "|"))))

;; review: t -> cond
;; review: otherwise -> case

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (let ((x (parse-integer str :junk-allowed t)))
	       (if x
		   (loop for i
			below x
			do (update-world)
			if (zerop (mod i 1000)) ;; if 1000 times passed, print "." 
			do (princ #\.))
		   (update-world))
	       (evolution))))))

;; :junk-allowed ;; some -> nil / 1112nil -> 1112
 
