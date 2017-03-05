;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; dice of doom 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 3) 
(defparameter *board-hexnum* (* *board-size* *board-size*)) 

;; clean
(defun board-array (lst)
  (make-array *board-hexnum* :initial-contents lst)) 	;; 1 to 3

;; #((1 3) (0 3) (0 1) (0 1))

;; clean
(defun gen-board ()
  (board-array (loop for n below *board-hexnum*
		    collect (list (random *num-players*)
				  (1+ (random *max-dice*))))))
;; #((1 3) (0 3) (0 1) (0 1))

;; clean
(defun player-letter (n)
  (code-char (+ 97 n)))
;; (player-letter 0) -> #\a

;; dirty
(defun draw-board (board)
  (loop for y below *board-size*	;; y means line-num
       do (progn (fresh-line)
		 (loop repeat (- *board-size* y)
		      do (princ "  "))  ;; slide by line-num
		 (loop for x below *board-size*	;; x means column-num 
		      for hex = (aref board (+ x (* *board-size* y))) 	;; look -> 0 1 2 \n 3 4 5 \n 6 7 8  
		      do (format t "~a-~a " (player-letter (first hex))  	;; hex = (0 3)
				 (second hex))))))

;; (draw-board #((0 3) (0 3) (1 3) (1 3)))
;;     a-3 a-3
;;   b-3 b-2

;; clean
;;(defun game-tree (board player spare-dice first-move)
;;  (list player
;;	board
;;	(add-passing-move board
;;			  player
;;			  spare-dice
;;			  first-move
;;			  (attacking-moves board player spare-dice))))
;; arguments
;;  - board array
;;  - player's num
;;  - dices getting by the player
;;  - is this the first play ?

;; clean
(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move 
      moves		;; return moves from #'attacking-moves	
      (cons (list nil	;; nil means turn-end
		  (game-tree (add-new-dice board player (1- spare-dice));; start by next-player / add-new-dice 
			     (mod (1+ player) *num-players*)		;; player change
			     0						;; spare-dice reset
			     t))					;; it is first move
	    moves)))

;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; if moves == nil and first-move == T :	;;
;;  	return nil	(means game-end)	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clean
(defun attacking-moves (board cur-player spare-dice)	;; get all plans to attack
  (labels ((player (pos)
	     (car (aref board pos)))
	   (dice (pos)
	     (cadr (aref board pos))))
    (mapcan (lambda (src)
	      (when (eq (player src) cur-player)
		(mapcan (lambda (dst)
			  (when (and (not (eq (player dst) cur-player)) 
				     (> (dice src) (dice dst)))	
			    (list
			     (list (list src dst) 	;; attack src -> dst
				   (game-tree (board-attack board cur-player	;; board transition
							    src dst (dice src))
					      cur-player		;; keep playing
					      (+ spare-dice (dice dst))	;; + got dice
					      nil)))))	;; not first attack
			(neighbors src))))
	    (loop for n below *board-hexnum*
		 collect n))))

;; clean
;;(defun neighbors (pos)	;; get all positions around pos 
;;  (let ((up (- pos *board-size*))	;; upper stage
;;	(down (+ pos *board-size*))) 	;; lower stage
;;    (loop for p in (append (list up down)
;;			   (unless (zerop (mod pos *board-size*))
;;			     (list (1- up) (1- pos)))
;;			   (unless (zerop (mod (1+ pos) *board-size*))
;;			     (list (1+ pos) (1+ down))))
;;	 when (and (>= p 0) (< p *board-hexnum*))
;;	 collect p)))

;; (neighbors 2) -> (0 3)

;; clean
(defun board-attack (board player src dst dice) ;; dice means dice-num of src
  (board-array (loop for pos from 0
		    for hex across board
		    collect (cond ((eq pos src) (list player 1))
				  ((eq pos dst) (list player (1- dice)))
				  (t hex))))) ;; keep dice-num

;; (board-attack #((0 3) (0 3) (1 3) (1 1)) 0 1 3 3) -> #((0 3) (0 1) (1 3) (0 2))

;; clean
;;(defun add-new-dice (board player spare-dice) ;; it calls every each player's-turn end
;;  (labels ((f (lst n)	;; get board as a list / n means spare-dice
;;	     (cond ((null lst) nil)
;;		   ((zerop n) lst)
;;		   (t (let ((cur-player (caar lst)) 	;; the player which has the pos
;;			    (cur-dice	(cadar lst)))	;; these dice-num
;;			(if (and (eq cur-player player) (< cur-dice *max-dice*))
;;			    (cons (list cur-player (1+ cur-dice))
;;				  (f (cdr lst) (1- n)))
;;			    (cons (car lst) (f (cdr lst) n))))))))
;;    (board-array (f (coerce board 'list) spare-dice))))


;;  (GAME-TREE #((0 1) (1 1) (0 2) (1 1)) 0 0 t)
;; ->
;; (0  					;; player
;;	#((0 1) (1 1) (0 2) (1 1))	;; board
;;	(((2 3)				;; moves
;; 	(0 				;; player
;;		#((0 1) (1 1) (0 1) (0 1)) ;; board
;; 		((NIL 			;; player change (this means a kind of moves)
;; 		(1   
;;			#((0 1) (1 1) (0 1) (0 1)) 
;;			NIL)))))))	;; end (player 1 lose by having region) ;; first-move and nil moves 
;;					;; this NIL means no moves 
;;

;; dirty
(defun player-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
      (player-vs-human (handle-human tree))	
      (announce-winner (cadr tree)))) 		;; argument tree player

;; dirty
(defun print-info (tree)
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree))) ;; 0 -> #\a
  (draw-board (cadr tree)))

;; dirty
(defun handle-human (tree)
  (fresh-line)
  (princ "choose your move: ")
  (let ((moves (caddr tree)))
    (loop for move in moves 	;; ex. ((2 3) (player #(board) ((move (player ...) (move (player ...) ...)))))
	 for n from 1
	 do (let ((action (car move))) 	;; ex. (2 3) 
	      (fresh-line)
	      (format t "~a. " n)
	      (if action
		  (format t "~a -> ~a" (car action) (cadr action))
		  (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves)))) ;; tree ex. (player ...)

;; clean
(defun winners (board)
  (let* ((tally (loop for hex across board
		     collect (car hex))) 	;; collect info between pos and player
	 (totals (mapcar (lambda (player)
			   (cons player (count player tally))) ;; ((player poses) (player poses) ...)
			 (remove-duplicates tally)))	;; ex. (1 1 1 0 0) -> (1 0) 
	 (best (apply #'max (mapcar #'cdr totals))))	;; ex. 3 
    (mapcar #'car
	    (remove-if (lambda (x)
			 (not (eq (cdr x) best)))
		       totals))))	;; return winner's list ex. (0)

;; dirty
(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
	(format t "The game is a tie between ~a" (mapcar #'player-letter w))
	(format t "The winner is ~a" (player-letter (car w))))))

;; clean
;;(defun rate-position (tree player)
;;  (let ((moves (caddr tree)))
;;    (if moves				;; have any moves 
;;	(apply (if (eq (car tree) player)	;; which player ? A / B
;;		   #'max
;;		   #'min)		
;;	       (get-ratings tree player))
;;	(let ((w (winners (cadr tree))))
;;	  (if (member player w)
;;	      (/ 1 (length w))			;; if there is two winner, it will return 0.5
;;	      0)))))				;; (/ 1 0) -> 0
;; -> 0 or 1 or 0.5

;; dirty
(defun get-ratings (tree player)
  (mapcar (lambda (move)
	    (rate-position (cadr move) player))
	  (caddr tree))) ;; moves list ex. (((move) (player #(board) ...) ((move) (player #(board) ...))
;; -> (0 1 0 0.5 1 ...)

;; clean
(defun handle-computer (tree)
  (let ((ratings (get-ratings tree (car tree))))			;; (car tree) -> player
    (cadr (nth (position (apply #'max ratings) ratings) (caddr tree)))));; (caddr tree) -> moves' list
;; -> tree ex. (player #(board) (moves... ))

;; dirty
(defun player-vs-computer (tree)
  (print-info tree)
  (cond ((null (caddr tree)) (announce-winner (cadr tree))) ;; no move -> winner judge by board
	((zerop (car tree)) (player-vs-computer (handle-human tree))) ;; 0 is player's id
	(t (player-vs-computer (handle-computer tree)))))	;; others are computer's turn


;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;
;; null-test 			;;
;; (null (list null #(1 2 3)))	;;
;; -> nil			;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
	(setf (gethash rest previous) (apply old-game-tree rest)))))

;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; apply gives rest as some arguments	;;
;; funcall fives rest as one argument 	;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

