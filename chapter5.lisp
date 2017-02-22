;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; base : game "magic-caster's-home" ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *nodes* '((living-room (you are in the living-room.
				      a wizard is snoring loudly on the couch.))
			(garden (you are in a beautiful garden.
				 there is a well in fornt of you.))
			(attic (you are i the attic.
				there is a giant welding torch in the corner.))))

(assoc 'garden *nodes*) ;; -> (garden (you are in a beautiful garden. there is a well in front of you.))

(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

(describe-location 'living-room *nodes*) ;; -> (you are in the living room. A wiz...)

(defparameter *edges* '((living-room 	(garden west door) ;; door east : garden west
			 		(attic upstairs ladder))
			(garden 	(living-room east door))
			(attic 		(living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.)) ;;!!! ' -> ` !!!

(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; tips
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(cdr (assoc 'living-room *edges*))	

(mapcar #'describe-path '((garden west door) (attic upstairs ladder)))
;; (describe-path '(garden west door)) (describe-path (attic upstairs ladder))

(mapcar #'sqrt '(1 2 3 4 5))
;; (sqrt 1) (sqrt 2) (sqrt 3)...

(mapcar (function car) '((foo bar) (baz qux))) ;; (foo baz)

(let ((car "Honda Civic"))		 ;; car as variable
  (mapcar #'car '((foo bar) (baz qux)))) ;; car as function

(append '(mary had) '(a) '(little lamb)) ;; (mary had a little lamb)

(apply #'append '((mary had) (a) (little lamb))) ;; (mary had a little lamb)

(apply #'append '((there is a door going west here.)
		  (there is a ladder going upstairs from here.)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *objects* '(whiskey bucket frog chain))
 
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

(defun objects-at (loc objs obj-locs)
  (labels ((at-loc-p (obj)
	     (eq (cadr (assoc obj obj-locs)) loc))) ;; does the loc have anything? 
    (remove-if-not #'at-loc-p objs)))  		    ;; the something in the list

(objects-at 'living-room *objects* *object-locations*) ;; (whikey bucket)

(defun describe-objects (loc objs obj-locs) 
  (labels ((describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs))))) 

(describe-objects 'living-room *objects* *object-locations*) 
;;(you see a whiskey on the floor. you see a bucket on the floor)

(defparameter *location* 'living-room)

(defun look ()
  (append 	(describe-location *location* *nodes*)
   		(describe-paths *location* *edges*)
		(describe-objects *location* *objects* *object-locations*)))

(defun walk (direction)
  (let ((next (find direction
		    (cdr (assoc *location* *edges*))
		    :key #'cadr))) ;; does the list has the key in the (cadr list)
    (if next
	(progn (setf *location* (car next))
	       (look))
	'(you cannot go that way.))))

(find 'y '((5 x) (3 y) (7 z) (8 y)) :key #'cadr) ;; -> (3 y) !same behavior : assoc

(defun pickup (object)
  (cond ((member object
		 (objects-at *location* *objects* *object-locations*))
	 (push (list object 'body) *object-locations*)
	 `(you are now carrying the ,object)) 
	(t '(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;!!!
(objects-at *location* *objects* *object-locations*) ;; (whiskey bucket)

(defparameter *object* 'bucket)
(pickup *object*) ;; (you are now carrying the bucket)

(objects-at *location* *objects* *object-locations*) ;; (whiskey) 
;;!look "(eq (cadr (assoc obj obj-locs)) loc))"
;; obj-locs -> ((bucket body) (whiskey living-room) (bucket livingroom) ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nodes
;; association list & assoc 
;; '  & ` , (Quasi-quart)
;; mapcar & append etc... (Higher order function) 
;; how to push something into association list 
;;      (find 'i '((3 i) (2 j) (6 k) (9 i)) :key #'cadr) -> O (3 i) 
;;							    X ((3 i) (9 i))
;;      (assoc 'i '((i something) (i others) (i extra))) -> O (i something)

;; finish in 2/21 2017
