;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; orc battle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; orc-battle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *player-health* nil)
(defparameter *player-agility* nil)
(defparameter *player-strength* nil)

(defparameter *monsters* nil)
(defparameter *monster-builders* nil)
(defparameter *monster-num* 12)

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
	     (lambda (x)
	       (declare (ignore x)) ;; avoid error
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
    (decf *player-health* x))
  (fresh-line))

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
    (fresh-line)
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
      (decf *player-health*))
    (fresh-line))) ;; decf reduce 1 from the element automatically

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
	   (decf *player-strength* 2))))
  (fresh-line))
