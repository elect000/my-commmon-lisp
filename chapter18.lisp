;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; lazy programing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro lazy (&body body)
  (let ((forced (gensym)) ;; unbrock name
	(value (gensym)))  ;; unblock name ;; created at function-called ;; it stored after called it.
	`(let ((,forced nil) ;; created at this compiled/declared
	       (,value nil))
	   (lambda ()			;; if funcall 
	     (unless ,forced 		;; it was calculated?
	       (setf ,value (progn ,@body)) ;; calculate it
	       (setf ,forced t)) 	    ;; it was calculated
	     ,value))))		;; it had been calculated, return it.

(defun force (lazy-value)
  (funcall lazy-value)) ;; call lambda-func


(defun add (a b) (princ "I am adding now") (+ a b))

(defparameter *foo* (lazy (add 1 2)))

(force *foo*)

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defparameter *integers*
  (labels ((f (n)
	     (lazy-cons n (f (1+ n)))))
    (f 1)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(lazy-cdr *integers*)

;; #<FUNCTION :LAMBDA NIL
;; (UNLESS #:G3951 (SETF #:G3952 (PROGN (CONS N (F (1+ N))))) (SETF #:G3951 T))
;; #:G3952>

;; because ...
;; *integers* = (lazy-cons 1 (lazy-cons 2 (lazy-cons 3 (lazy-cons ...))))

(defun make-lazy (lst)
  (lazy (when lst 
	  (cons (car lst) (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(take 10 *integers*)

(take 10 (make-lazy '(q w e r t y u i o p a s d f)))

(take-all (make-lazy '(q w e r t y u i o p a s d f)))

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
	  (cons (funcall fun (lazy-car lst))
		(lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur) ;; current element
	     (if (lazy-null lst-cur) ;; it's nil (not match the conditions)
		 (force (lazy-mapcan fun (lazy-cdr lst))) ;; deal cdr-list
		 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur))))))) ;; process: elements in an element
    (lazy (unless (lazy-null lst)
	    (f (funcall fun (lazy-car lst))))))) 
;; this func must return nil in no match element
;; (mapcan  (lambda (x) (if (evenp x) (list x) (list 'yup 'yup)))  '(1 2 3 4 5))
;; (YUP YUP 2 YUP YUP 4 YUP YUP)

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    	(let ((x (lazy-car lst)))
	  (if (funcall fun x)
	      x
	      (lazy-find-if fun (lazy-cdr lst))))))
;; this func must return nil in no match element

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))

(take 10 (lazy-mapcar #'sqrt *integers*))

(take 10 (lazy-mapcan (lambda (x) ;; lambda-func must return list
			(if (evenp x)
			    (make-lazy (list x));; lazy list in one element
			    (lazy-nil)))	;;  - lazy cannot make element
		      *integers*))

(lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10)))

(lazy-nth 4 (make-lazy '(a b c d e f g))) ;; E

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; dice_of_doom_v2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "dice_of_doom_v1.lisp")

(load "lazy.lisp")

(defparameter *board-size* 4)

(defparameter *board-hexnum* (* *board-size* *board-size*)) 

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil ;;  play ex. (2 3) / (1 2) ... nil
		       (game-tree (add-new-dice board player
						(1- spare-dice)) ;; board
				  (mod (1+ player) *num-players*);; player
				  0				;; spare-dice
				  t))				;; first-move
		 moves)))


(defun attacking-moves (board cur-player spare-dice) ;; attack src -> dst
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (lazy-mapcan 
     (lambda (src)
       (if (eq (player src) cur-player)
	   (lazy-mapcan
	    (lambda (dst)
	      (if (and (not (eq (player dst) cur-player))
		       (> (dice src) (dice dst)))
		  (make-lazy
		   (list (list (list src dst)
			       (game-tree (board-attack board      ;; where
							cur-player ;; who
							src	   ;; from
							dst	   ;; to
							(dice src));; info
					  cur-player
					  (+ spare-dice (dice dst))
					  nil)))) ;; non first move
		  (lazy-nil))) ;; not match these conditions
	    (make-lazy (neighbors src))) ;; dst <- neighbors src
	   (lazy-nil))) ;; it's not cur player's one
     (make-lazy	(loop for n below *board-hexnum* ;; src <- all pos
		     collect n)))))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move: ")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
	       (unless (lazy-null moves)
		 (let* ((move (lazy-car moves))
			(action (car move)))
		   (fresh-line)
		   (format t "~a. " n)
		   (if action
		       (format t "~a -> ~a" (car action) (cadr action))
		       (princ  "end turn"))) ;; no action
		 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun play-vs-human (tree)
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree)) ;; 
      (announce-winner (cadr tree)))) ;; board <- board

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; game-AI ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun limit-tree-depth (tree depth)
  (list (car tree)
	(cadr tree)
	(if (zerop depth)
	    (lazy-nil) ;; if zero -> nil
	    (lazy-mapcar (lambda (move)
			   (list (car move)
				 (limit-tree-depth (cadr move) (1- depth)))) ;; recursive
			 (caddr tree))))) ;; moves

(defparameter *ai-level* 4)

(defun handle-computer (tree)
  (let ((ratings (get-ratings (limit-tree-depth tree *ai-level*) ;; moves list
			      (car tree))))			;; player
    (cadr (lazy-nth (position (apply #'max ratings) ratings)
		    (caddr tree))))) ;; moves

(defun play-vs-computer (tree)
  (print-info tree)
  (cond ((lazy-null (caddr tree)) (announce-winner (cadr tree))) ;; board
	((zerop (car tree)) (play-vs-computer (handle-human tree))) ;; no.0 is player's name
	(t (play-vs-computer (handle-computer tree)))))	;; no-game-end and player = no.1(AI)

(defun score-board (board player)
  (loop for hex across board
       for pos from 0
       sum (if (eq (car hex) player)
	       (if (threatened pos board)
		   1	;; the player(AI) has it which is nearby enemy's pos
		   2)	;; the player(AI) has it which is not so
	       -1)))	;; it is owned by enemy

(defun threatened (pos board)
  (let* ((hex (aref board pos))
	 (player (car hex))
	 (dice (cadr hex)))
    (loop for n in (neighbors pos)
	 do (let* ((nhex (aref board n))
		   (nplayer (car nhex))
		   (ndice (cadr nhex)))
	      (when (and (not (eq player nplayer)) (> ndice dice)) ;;enemy's pos which has many dices
		(return t)))))) ;; exit from the loop

(defun get-ratings (tree player)
  (take-all (lazy-mapcar (lambda (move)
			   (rate-position (cadr move) player))
			 (caddr tree))))

(defun rate-position (tree player)
  (let ((move (caddr tree)))
    (if (not (lazy-null move))
	(apply (if (eq (car tree) player)
		   #'max
		   #'min)
	       (get-ratings tree player))	;; list of same generation's value ex. (child child child ...)
	(score-board (cadr tree) player))))	;;  child is contained grandson's value ex. #'max(gr gr gr ...)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; alpha and beta algolism ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
(defun ab-get-ratings-max (tree player upper-limit lower-limit)
  (labels ((f (moves lower-limit)
	     (unless (lazy-null moves)
	       (let ((x (ab-rate-position (cadr (lazy-car moves)) ;; tree 
					  player
					  upper-limit
					  lower-limit)))
	       (if (>= x upper-limit)
		   (list x)
		   (cons x (f (lazy-cdr moves) (max x lower-limit))))))))
    (f (caddr tree) lower-limit)))

;; moves
;; ((from to)
;; 	(TREE))
;; tree
;; (player
;;  board
;;  MOVES)
c
(defun ab-get-ratings-min (tree player upper-limit lower-limit)
  (labels ((f (moves upper-limit)
	     (unless (lazy-null moves)
	       (let ((x (ab-rate-position (cadr (lazy-car moves))
					  player
					  upper-limit
					  lower-limit)))
		 (if (<= x lower-limit)
		     (list x) 
		     (cons x (f (lazy-cdr moves) (min x upper-limit))))))))
    (f (caddr tree) upper-limit)))

(defun ab-rate-position (tree player upper-limit lower-limit)
  (let ((moves (caddr tree)))
    (if (not (lazy-null moves))
	(if (eq (car tree) player)
	    (apply #'max (ab-get-ratings-max tree
					     player
					     upper-limit
					     lower-limit))
	    (apply #'min (ab-get-ratings-min tree
					     player
					     upper-limit
					     lower-limit)))
	(score-board (cadr tree) player))))

(defun handle-computer (tree)
  (let ((ratings (ab-get-ratings-max (limit-tree-depth tree *ai-level*)
				     (car tree)
				     most-positive-fixnum
				     most-negative-fixnum)))
    (cadr (lazy-nth (position (apply #'max ratings) ratings) (caddr tree)))))

(defparameter *board-size* 5)
(defparameter *board-hexnum* (* *board-size* *board-size*))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; delayed programing is useful in too large problem  (you can watch small pieces of it)
;; - we can use "lazy" and "force" macro
;; hulistic means it is not correct but it may be correct. it can make program more fast
;; ex. delay tree let us play the game in more large game board
;; alpha-beta method improve efficiency


