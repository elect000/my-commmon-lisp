;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; dice of doom ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;; dice position is position
;; 	element (player dice-num)

;; clean
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst))

;; dirty
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
		    collect (list (random *num-players*)       	;; 0 or 1
				  (1+ (random *max-dice*))))))	;; 1 to 3
;; #((1 3) (0 3) (0 1) (0 1))

;; clean
(defun player-letter (n)
  (code-char (+ 97 n))) 

;; (player-letter 0) -> #\a

;; dirty
(defun draw-board (board)
  (loop for y below *board-size*  ;; ex. 0 1 ...
       do (progn (fresh-line)
		 (loop repeat (- *board-size* y) ;; ex. 2-0 2-1 ...
		      do (princ "  "))
		 (loop for x below *board-size* ;; ex. 0 1 ... (colmn)
		      for hex = (aref board (+ x (* *board-size* y))) ;; (* *board...) means line-num (line)
		      do (format t "~a-~a " (player-letter (first hex)) ;; hex means elemet such as (1 3)
				 (second hex))))))
;; (draw-board #((0 3) (0 3) (1 3) (1 3)))
;;     a-3 a-3
;;   b-3 b-2

;; clean
(defun game-tree (board player spare-dice first-move) 
  (list player
	board  
	(add-passing-move board 
			  player
			  spare-dice
			  first-move
			  (attacking-moves board player spare-dice))))
;; arguments 
;; - state of board
;; - player's attribute
;; - dice getting by the player (for serve)
;; - this is the first play or not

;; clean
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move  ;; is this first move?
      moves	  ;; player should move at least 1 time.
      (cons (list nil ;; this means player's move. this func is for change player, so this is nil 
		  (game-tree (add-new-dice board player (1- spare-dice)) ;; create new board with supply dice
			     (mod (1+ player) *num-players*)		;; player change
			     0						;; changed player has not dices yet
			     t))					;; it is his/her first play
	    moves)))

;; clean
(defun attacking-moves (board cur-player spare-dice) ;; cur-player = current player
  (labels ((player (pos) ;; ex. 0 1... (player-id)
	     (car (aref board pos)))
	   (dice (pos)   ;; ex. 1 2 3... (dice-num)
	     (cadr (aref board pos))))
    (mapcan (lambda (src)	;; ex. 0 1 2 3 ... (pos)
	      (when (eq (player src) cur-player) ;; the pos was owned by the player
		(mapcan (lambda (dst)		;; neighbor pos
			  (when (and (not (eq (player dst) cur-player)) ;; it is not owned by the player 
				     (> (dice src) (dice dst)))	        ;; its dice-num is less than that num
			    (list 
			     (list (list src dst)			;; player's pos and attacked pos
				   (game-tree (board-attack board cur-player  	;; #'board-attack -> board
							    src dst (dice src))
					      cur-player			;; player
					      (+ spare-dice (dice dst))	;; spare dice increment
					      nil)))))	;; not first attack
			(neighbors src))))
	    (loop for n below *board-hexnum* ;; get all pos
	       		collect n))))

;; clean
(defun neighbors (pos)
  (let ((up (- pos *board-size*))   	;;  0 1
	(down (+ pos *board-size*))	;; 2 3 .. 0 = 2 - 2
	)
    (loop for p in (append (list up down) 
			   (unless (zerop (mod pos *board-size*))  ;;  0 1 
			     (list (1- up) (1- pos)))              ;; 2 3 ... 0 = 1-1 / 2 = 3-1
 			   (unless (zerop (mod (1+ pos) *board-size*)) ;;  0 1
			     (list (1+ pos) (1+ down))))	       ;; 2 3 ... 1 = 0+1 / 3 = 2+1
	 when (and (>= p 0) (< p *board-hexnum*)) ;; 0 <= p < box-num 
	 collect p)))

;; (neighbors 2) -> (3 0)

;; clean
(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0
		    for hex across board ;; board is array
		    collect (cond ((eq pos src) (list player 1)) ;; pos = src -> there is one dice
				  ((eq pos dst) (list player (1- dice))) ;; pos = dst -> ...
				  (t hex))))) ;; keep dice-num

;; (board-attack #((0 3) (0 3) (1 3) (1 1)) 0 1 3 3)
;;  3 3     3 1
;; 3 1  -> 3 2

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)	;; get board as a list / n is spare-dice
	     (cond ((null lst) nil)
		   ((zerop n) lst)
		   (t (let ((cur-player (caar lst))
			    (cur-dice 	(cadar lst)))
			(if (and (eq  cur-player player ) (< cur-dice *max-dice*))
			    (cons (list cur-player (1+ cur-dice))
				  (f (cdr lst) (1- n)))
			    (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))

;;  (GAME-TREE #((0 1) (1 1) (0 2) (1 1)) 0 0 t)
;; ->
;; (0  					;; player
;;	#((0 1) (1 1) (0 2) (1 1))	;; board
;;	(((2 3)				;; moves
;; 	(0 				;; player
;;		#((0 1) (1 1) (0 1) (0 1)) ;; ...
;; 		((NIL 			;; player change
;; 		(1   
;;			#((0 1) (1 1) (0 1) (0 1)) 
;;			NIL)))))))	;; end (player 1 lose)
;;

;; dirties
(defun player-vs-human (tree)
  (print-info tree)
  (if (caddr tree) ;; have any game-status
      (player-vs-human (handle-human tree)) 
      (announce-winner (cadr tree)))) ;; get board info

(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree))) ;; 0 -> #\a
  (draw-board (cadr tree)))

(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move: ")
  (let ((moves (caddr tree)))
    (loop for move in moves
	 for n from 1
	 do (let ((action (car move))) ;; ex. (2 3)
	      (fresh-line)
	      (format t "~a. "  n)
	      (if action
		  (format t "~a -> ~a" (car action) (cadr action))
		  (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves)))) ;; return selected move

;; clean
(defun winners (board)
  (let* ((tally (loop for hex across board
		     collect (car hex))) ;; get information about player ex.(1 0 1 0 0 0 ...)
	 (totals (mapcar (lambda (player)
			   (cons player (count player tally))) ;; ex. ((0 4) (1 3))
			 (remove-duplicates tally))) ;; (1 0 1 0 0 0 0 0 0) -> (1 0)
	 (best (apply #'max (mapcar #'cdr totals)))) ;; the size of the largest region / not player
    (mapcar #'car
	    (remove-if (lambda (x)  ;; who is the best player? / it may be two players / x ex. (1 4)
			 (not (eq (cdr x) best))) 
		       totals))))

;;dirty
(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
	(format t "The game is a tie between ~a" (mapcar #'player-letter w))
	(format t "The winner is ~a" (player-letter (car w))))))


;; clean
(defun rate-position (tree player)
  (let ((moves (caddr tree))) ;; now, we have any plays
    (if moves			
	(apply (if (eq (car tree) player) ;; now, is player player or enemy?  
		   #'max		;; return max ;; advantage
		   #'min)		;;	      ;; disadvantage
	       (get-ratings tree player))
	(let ((w (winners (cadr tree)))) ;; we cannot play any more
	  (if (member player w) 
	      (/ 1 (length w)) ;; if there is two winner, it will return 0.5
	      0)))))		;; the player lose
;; -> 0 or 1 or 0.5 ...

;; dirty
(defun get-ratings (tree player) ;; map all tree
  (mapcar (lambda (move) ;; ex. ((1 3) (0 #((1 1) (0 1) (1 1) (0 1)) ((NIL (1 #((1 1) (0 1) (1 1) (0 1)) NIL)))))
	    (rate-position (cadr move) player))
	  (caddr tree))) ;; all plans
;; -> ex. (1 1 1) / (0) / (1/2)

;; clean
(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))))

;; dirty
(defun player-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree)))
	((zerop (car tree)) (player-vs-computer (handle-human tree)))
	(t (player-vs-computer (handle-computer tree)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speed up ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *board-size* 3)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;; closure
(defparameter *foo* (lambda ()
		      5))

(defparameter *foo* (let ((x 5))
		     (lambda ()
		       x)))

(let ((line-number 0))
  (defun my-print (x)
    (print line-number)
    (print x)
    (incf line-number)
    nil))
;; line-number was caught by closure
;;  so, it can store (can count up)

;; memolize

(neighbors 0)
;;-> (3 1 4)

(defun neighbors (pos)	;; get all positions around pos 
  (let ((up (- pos *board-size*))	;; upper stage
	(down (+ pos *board-size*))) 	;; lower stage
    (loop for p in (append (list up down)
			   (unless (zerop (mod pos *board-size*))
			     (list (1- up) (1- pos)))
			   (unless (zerop (mod (1+ pos) *board-size*))
			     (list (1+ pos) (1+ down))))
	 when (and (>= p 0) (< p *board-hexnum*))
	 collect p)))

(let ((old-neighbors (symbol-function 'neighbors)) 
      (previous (make-hash-table))) ;; key = pos(number) / value = result
  (defun neighbors (pos)
    (or (gethash pos previous)		;; access success -> get value
	(setf (gethash pos previous) (funcall old-neighbors pos))))) ;; access fail -> add new info

;; symbol-function get previous neighbors

(defun game-tree (board player spare-dice first-move) 
  (list player
	board  
	(add-passing-move board 
			  player
			  spare-dice
			  first-move
			  (attacking-moves board player spare-dice))))

(let ((old-game-tree (symbol-function 'game-tree))
      (previous (make-hash-table :test #'equalp))) ;; key = player board ... / value tree
  (defun game-tree (&rest rest)
    (or (gethash rest previous)
	(setf (gethash rest previous) (funcall old-game-tree rest)))))

;; review 
(eql 2 2) ;; -> T
(eql #(1 2) #(1 2)) ;; -> nil
(equalp #(1 2) #(1 2)) ;; -> T


(defun rate-position (tree player)
  (let ((moves (caddr tree))) ;; now, we have any plays
    (if moves			
	(apply (if (eq (car tree) player) ;; now, is player player or enemy?  
		   #'max		;; return max ;; advantage
		   #'min)		;;	      ;; disadvantage
	       (get-ratings tree player))
	(let ((w (winners (cadr tree)))) ;; we cannot play any more
	  (if (member player w) 
	      (/ 1 (length w)) ;; if there is two winner, it will return 0.5
	      0)))))		;; the player lose

(let ((old-rate-position (symbol-function 'rate-position))
      (previous (make-hash-table)))
  (defun rate-position (tree player)
    (let ((tab (gethash player previous)))
      (unless tab ;; value of the key :player = nil
	(setf tab (setf (gethash player previous) (make-hash-table))))
      (or (gethash tree tab)
	  (setf (gethash tree tab)
		(funcall old-rate-position tree player))))))

;; hash-list (previous)	... key = player 		/ value = tab (hash-list)
;;   - hash-list (tab)	... key = tree(memolized)   	/ value = rate-position
;;
;; it has two hash-lists

;; optimisation
(defun my-length (lst)
  (if lst
      (1+ (my-length (cdr lst)))
      0))

(my-length '(fie foh fum))

(defparameter *biglist* (loop for i below 100000 collect 'x))

;; (my-length *biglist*) ;; clisp broken

(defun my-length (lst)
  (labels ((f (lst acc)
	     (if lst
		 (f (cdr lst) (1+ acc))
		 acc)))
    (f lst 0)))

(my-length *biglist*) ;; clisp not broken

;; because ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; before ... called multiple func one time		    		;;
;; after  ... called single func one time (info was stored in argument)	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n acc)			;; acc = board-array's list ex. ( tail  ... head )
 	     (cond ((zerop n) (append (reverse acc) lst)) ;; cf. (append (reverse (2 1)) (3 4 5))
		   ((null lst) (reverse acc))
		   (t (let ((cur-player (caar lst))
			    (cur-dice (cadar lst)))
			(if (and (eq cur-player player)
				 (< cur-dice *max-dice*))
			    (f (cdr lst)	;; next-pos
			       (1- n)		;; spare-dice
			       (cons (list cur-player (1+ cur-dice)) acc))  ;; acc 
			    (f (cdr lst) n (cons (car lst) acc))))))))	
    (board-array (f (coerce board 'list) spare-dice ()))))

;; (append (reverse '()) 'hoge) -> hoge
;; (car '(#(1 2 3))) -> #(1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; game tree is thanks for pipeline
;; 	pipeline can make independent development among functional programing
;; mini-max algorithm is useful in two player's game (if it is good for one, it is bad for another)
;; lexical scope can catch variables by lambda-func : it's called closure
;; 


;; defvar/parameter make special variable (= global scope)
;; (defvar y 100)
;; (defparameter hoge (let ((y 10)) (lambda () y)))
;; (funcall hoge) -> 100
;; it can break lexical scope, so you should make special variable with "**"
;; functional programing can speed up by using memolize (remembering results)
;; list-recursion is have a lot of stack, so you use otpimisation of it with acculater (store state).
;;   ex. (defun my-length (lst)
;;		(labels ((f (lst acc)
;;				(if lst
;;					(f (cdr lst) (1+ acc))
;;					acc)))
;;			(f lst 0))
;;		;; acc is store info about length

