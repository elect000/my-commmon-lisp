;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; orc battle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; how to use array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-array 3)
;; -> #(0 0 0)

(make-array 3 :initial-element nil)
;; -> #(MIL NIL NIL)

(defparameter x (make-array 3))
;; -> #(0 0 0)

(setf (aref x 1) 'foo)
;; #(0 FOO 0)

(aref x 1)
;; FOO

(defparameter foo (list 'a 'b 'c))
;; -> (A B C)

(second foo)
;; -> B

(setf (second foo) 'z)
;; -> (A Z C)

(setf foo (make-array 4 :initial-element nil))
;; -> (NIL NIL NIL NIL)

(setf (aref foo 2) (list 'x 'y 'z))
;; -> (NIL NIL (X Y Z) NIL)

(setf (car (aref foo 2)) (make-hash-table))

(setf (gethash 'zoink (car (aref foo 2))) 5)
;; (NIL NIL (S#(HASH-TABLE (ZOINK . 5)) Y Z) NIL)

;; how to use list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nth 1 '(foo bar baz))
;; bar

;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speed of array or list                         ;;
;; array can access too far date faster           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; how to use hashtable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-hash-table)

(defparameter x (make-hash-table))

(gethash 'yup x) 
;; -> NIL ... key number
;;    NIL ... not exist  
;; because there is nothing in the hash-table

(defparameter x (make-hash-table))

(setf (gethash 'yup x) '25) ;; set

(gethash 'yup x) 
;; -> 25 ... key number 
;;    T  ... exist

(defparameter *drink-order* (make-hash-table))

(setf (gethash 'bill *drink-order*) 'double-espresso)

(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)

(setf (gethash 'john *drink-order*) 'medium-latte)

(gethash 'lisa *drink-order*)

;; some functions return some value ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(round  2.4)
;; -> 2
;;    0.4

(defun foo ()
  (values 3 7))
;; -> 3
;;    7

(+ (foo) 5)
;; 8
;; use first value

(multiple-value-bind (a b) (foo)
  (* a b))
;; a = 3
;; b = 7
;; -> 21

(load "chapter8")



;; override
(defun hash-edges (edge-list) ;; ex ((1 . 2) (3 . 4) ...)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
	    (let ((node (car x)))
	      (push (cdr x) (gethash node tab))))
	  edge-list)
    tab))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table))) 		;; initialize
    (labels ((traverse (node)			;; from node to anywhere
	       (unless (gethash node visited)	;; no entry in the hash table = not visiting
		 (setf (gethash node visited) t) ;; add entry t in the key
		 (mapc (lambda (edge)		
			 (traverse edge))	   
		       (gethash node edge-tab))))) ;; search all edges from the node 
      (traverse node))
    visited)) ;; return 

;;(time (dotimes (i 500000) (get-connected 1 (make-edge-list))))
;; nearly 13 seconds

;;(time (dotimes (i 500000) (get-connected-hash 1 (hash-edges (make-edge-list))))) 				
;; nearly 8 seconds

;; data structure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct 	person
  		name
		age
		waist-size
		favorite-color)

(defparameter *bob* (make-person :name "Bob"
				 :age 35
				 :waist-size 32
				 :favorite-color "blue"))

;; make-,(data-structure) is defined automatically
;; -> (PERSON :NAME "Bob" :AGE 35 ...)

(person-age *bob*)

;; -> 35
;; ,(data-structure)-slot is defined automatically

(setf (person-age *bob*) 36)


;; (defparameter *THAT-GUY* #S(PERSON :NAME "Bob" :AGE 36 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")) 
;; -> it is active in repl buffer so this cannot compile

;; (defun make-person (name age waist-size favorite-color)
;;     (list name age waist-size favorite-color))

;; (defun person-age (person)
;;   (cadr person))

;; (defparameter *bob* (make-person "bob" 35 32 "blue"))

;; (person-age *bob*)
;; -> 35

;; genelic function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sequence function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example
(length '(a b c)) ;; -> 3 / or (list-length '(a b c))

(length "blue")	  ;; -> 4

(length (make-array 5)) ;; -> 5

(find-if #'numberp '(a b 5 d)) ;; -> 5

(count #\s "mississippi") ;; -> 4

(position #\4 "2kewl4skewl") ;; -> 5

(some #'numberp '(a b 5 d)) ;; -> T

(every #'numberp '(a b 5 d)) ;; -> NIL

;; reduce
(reduce #'+ '(1 2 3 4)) ;; -> 10

(reduce #'cons '(a b c d)) ;; -> (((A . B) . C) . D)

(reduce (lambda (best item) 
	  (if (and (evenp item) (> item best))
	      item
	      best))
	'(7 4 6 5 2)
	:initial-value 0)

;; -> 6
;; 1st argument(best) <= initial-value / 2nd argument(item) <= 7

(reduce (lambda (best item) 
	  (if (and (evenp item) (> item best))
	      item
	      best))
	'(7 4 6 5 2))	

;; -> 7
;; 1st argument(best) <= 7 / 2nd argument(item) <= 4

(defun sum (lst)
  (reduce #'+ lst))

(sum '(1 2 3)) ;; -> 6

(sum (make-array 5 :initial-contents '(1 2 3 4 5))) ;; -> 15

;; (sum "blablabla") ;; -> error

(map 'list
     (lambda (x)
       (if (eq x #\s)
	   #\S
	   x))
     "this is a string")

;; -> (#\t #\h #\i #\S #\  #\i #\S #\  #\a #\  #\S #\t #\r #\i #\n #\g)

(map 'string
     (lambda (x)
       (if (eq x #\s)
	   #\S
	   x))
     "this is a string")

;; -> "thiS iS a String"


;; subseq

(subseq "america" 2 6)

;; sort

(sort '(5 4 2 1 3) #'<)

;; Type ideom

(numberp 5)

(characterp #\a)

(stringp "Hey")

(consp '(list a b))

(functionp (lambda (x) (* x x)))

(hash-table-p (make-hash-table))

(arrayp #(1 2 3)) 

(listp '(a b c))

(symbolp 'bob)

;; -> T

(defun add (a b)
  (cond ((and  (numberp a) (numberp b)) 
	 (+ a b))
	((and (listp a) (listp b))
	 (append a b)))) ;; append -> '(a b) / list -> ((a) (b))

(add 1 2) 	;; -> 3
(add '(1) '(2)) ;; -> (1 2)

(defmethod addm ((a number) (b number))
  (+ a b))

(defmethod addm ((a list) (b list))
  (append a b))

(addm 1 2) 	;; ->  3
(addm '(1) '(2)) ;; -> (1 2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; orc-battle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* nil)

(defun orc-battle ()
  (init-monsters)
  (init-player)
  (game-loop)
  (when (player-dead)
    (princ "You have been killed. Game Over."))
  (when (monsters-dead)
    (princ "Congratulations! You have vanquished all of your foes.")))

(defun game-loop ()
  (unless (or (player-dead) (monsters-dead))
    (show-player)
    (dotimes (k (1+ (truncate (/ (max 0 *player-agility*) 15)))) ;; *player-agility* % 15 + 1
      (unless (monsters-dead)
	(show-monsters)
	(player-attack)))
    (fresh-line)
    (map 'list	
	 (lambda (m)
	   (or (monster-dead m) (monster-attack m)))
	 *monsters*)
    (game-loop)))

;; dotimes
(dotimes (i 3)
  (fresh-line)
  (princ i)
  (princ ". Hatchoo!"))

(defun init-player ()
  (setf *player-health* 30)
  (setf *player-agility* 30)
  (setf *player-strength* 30))

(defun player-dead ()
  (<= *player-health* 0))

(defun show-player ()
  (fresh-line)
  (princ "You are a valiant knight with a helth of ")
  (princ *player-health*)
  (princ ", an agility of ")
  (princ *player-agility*)
  (princ ", and a strength of ")
  (princ *player-strength*))

(defun player-attack ()
  (fresh-line)
  (princ "Attack style: [s]tab [d]ouble swing [r]oundhouse: ")
  (case (read)
    (s (monster-hit (pick-monster)
		    (+ 2 (randval (ash *player-strength* -1)))))
    (d	(let ((x (randval (truncate (/ *player-strength* 6)))))
	  (princ "Your double swing has a strength of ")
	  (princ x)
	  (fresh-line)
	  (monster-hit (pick-monster) x)
	  (unless (monsters-dead)
	    (monster-hit (pick-monster) x))))
    (otherwise (dotimes (x (1+ (randval (truncate (/ *player-strength* 3)))))
		 (unless (monsters-dead)
		   (monster-hit (random-monster) 1))))))

;; randval

(defun randval (n)
  (1+ (random (max 1 n))))
;; (random x) -> 0 <= return <= x-1 

(dotimes (i 10)
  (princ (random 5))
  (princ " "))
;; -> 0 2 3 4 4 1 4 4 0 4 2

(defun random-monster ()
  (let ((m (aref *monsters* (random (length *monsters*))))) ;; 0 <= len <= length-1
    (if (monster-dead m) ;; the monster is dead ? 
	(random-monster)
	m)))						   

(defun pick-monster ()
  (fresh-line)
  (princ "Monster #:")
  (let	((x (read)))
    (if (not (and (integerp x) (>= x 1) (<= *monster-num*)))
	(progn	(princ "That is not a valid monster number.")
		(pick-monster))
	(let	((m (aref *monsters* (1- x))))
	  (if (monster-dead m)
	      (progn	(princ "That monster is already dead.")
			(pick-monster))
	      m)))))


(defun init-monsters ()
  (setf *monsters*
	(map 'vector
	     (lambda (cc)
	       (declare (ignore cc)) ;; avoid error
	       (funcall (nth (random (length *monster-builders*))
			     *monster-builders*))) 
	     (make-array *monster-num*))))

(defun monster-dead (m)
  (<= (monster-health m) 0))

(defun monsters-dead ()
  (every #'monster-dead *monsters*))

(defun show-monsters ()
  (fresh-line)
  (princ "Your foes: ")
  (let ((x 0))
    (map 'list
	 (lambda (m)
	   (fresh-line)
	   (princ "   ")
	   (princ (incf x)) ;; increment x 
	   (princ ". ")
	   (if (monster-dead m)
	       (princ "**dead**")
	       (progn	(princ "(Health=")
			(princ (monster-health m))
			(princ ")")
			(monster-show m))))
	 *monsters*)))

;; abstruct monster ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct monster (health (randval 10)))

(make-monster) ;; -> ex. #S(MONSTER :HEALTH 4)

(defmethod monster-hit (m x)
  (decf (monster-health m) x) ;; (monster-health m) = (monster-health  m) - x
  (if (monster-dead m)	;; access monster array
      (progn	(princ "You killed the ")
		(princ (type-of m))
		(princ "! "))
      (progn	(princ "You hit the ")
		(princ (type-of m))
		(princ ", knocking off ")
		(princ x)
		(princ " health points! "))))

;; how to use type-of
(type-of 'foo) ;; symbol
(type-of 5) ;; integer
(type-of "foo") ;; Array
(type-of (make-monster)) ;; monster

(defmethod monster-show (m)
  (princ "A fierce ")
  (princ (type-of m)))

(defmethod monster-attack (m))


;; defun about orc ;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (orc
	     (:include monster))
  (club-level (randval 8)))
		
(push #'make-orc *monster-builders*)

(defmethod monster-show ((m orc))
  (princ "A wicked orc with a level ")
  (princ (orc-club-level m))
  (princ " club"))

(defmethod monster-attack ((m orc))
  (let ((x (randval (orc-club-level m))))
    (princ "An orc swings hsi club at you and knocks off ")
    (princ x)
    (princ " of your health points. ")
    (decf *player-health* x)))

;; defun about hydra ;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (hydra (:include monster)))
(push #'make-hydra *monster-builders*)

(defmethod monster-show ((m hydra))
  (princ "A malicious hydra with ")
  (princ (monster-health m))
  (princ " heads."))

(defmethod monster-hit ((m hydra) x)
  (decf (monster-health m) x)
  (if (monster-dead m)
      (princ "The corpse of the fully decapitated and decapacitated hydra falls to the floor!")
      (progn (princ "You lop off ")
	     (princ x)
	     (princ " of the hydra's heads! "))))

(defmethod monster-attack ((m hydra))
  (let ((x (randval (ash (monster-health m) -1))))
    (princ "A hydra attacks you with ")
    (princ x)
    (princ " of its heads! It also grows back one more head! ")
    (incf (monster-health m))
    (decf *player-health* x)))

;; defun about slime ;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (slime-mold (:include monster)) (slimeness (randval 5)))
(push #'make-slime-mold *monster-builders*)

(defmethod monster-show ((m slime-mold))
  (princ "A slime mold with a slimeness of ")
  (princ (slime-mold-slimeness m)))

(defmethod monster-attack ((m slime-mold))
  (let ((x (randval (slime-mold-slimeness m))))
    (princ "A slime mold wraps around you legs and decreases your agility by ")
    (princ x)
    (princ "! ")
    (decf *player-agility* x)
    (when (zerop (random 2))
      (princ "It also squirts in your face, taking away a health point! ")
      (decf *player-health*)))) ;; decf reduce 1 from the element automatically

;; defun about brigand

(defstruct (brigand (:include  monster)))
(push #'make-brigand *monster-builders*)

(defmethod monster-attack ((m brigand))
  (let ((x (max *player-health* *player-agility* *player-strength*)))
    (cond ((= x *player-health*)
	   (princ "A brigand hits you with his slingshot, taking off 2 health points! ")
	   (decf *player-health* 2))
	  ((= x *player-agility*)
	   (princ "A brigand catches your leg with his whip, taking off 2 agility points! ")
	   (decf *player-agility* 2))
	  ((= x *player-strength*)
	   (princ "A brigand cuts your arm with his whip, taking off 2 strength points! ")
	   (decf *player-strength* 2)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; array & list
;; hashtable & alist (associate list)
;; if we use array and hashtable in correct points, the program will be faster
;; if we check the program's ability -> use time commands
;;    --- (time #'func)
;; how to use generic  ex. length add etc ,,,(sequence func) 
;; defstruct & defmethod
