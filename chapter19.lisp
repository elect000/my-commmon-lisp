;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; game :svg-format ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "dice_of_doom_v2.lisp")

(load "webserver.lisp")

(load "svg.lisp")

(defparameter *board-width* 900) ;; board-size (px)

(defparameter *board-height* 500) ;; board-size (px)

(defparameter *board-scale* 64) ;; 1 piece of image

(defparameter *top-offset* 3) ;; top's space 

(defparameter *dice-scale* 40) ;; dice size (px)

(defparameter *dot-size* 0.05) ;; dot's size

(defun draw-die-svg (x y col) ;;(x y) ... dice's pos / col means color
  (labels ((calc-pt (pt)	;; caluculate size (big or small)
	     (cons (+ x (* *dice-scale* (car pt))) ;; (* *dice-scale* (car pt)) ... scale fitness 	
		   (+ y (* *dice-scale* (cdr pt)))))
	   (f (pol col)
	     (polygon (mapcar #'calc-pt pol) col)))
    ;; base color (doesn't draw dice number)
    ;; top
    (f '((0 . -1) (-0.6 . -0.75) (0 . -0.5) (0.6 . -0.75)) ;; polygon region
       (brightness col 40))				   ;; color (bright)
    ;; left 
    (f '((0 . -0.5) (-0.6 . -0.75) (-0.6 . 0) (0 . 0.25))  ;; polygon
       col)						   ;; color (normal)
    ;; right
    (f '((0 . -0.5) (0.6 . -0.75) (0.6 . 0) (0 . 0.25))	   ;; polygon
       (brightness col -40))				   ;; color (dark)
    (mapc (lambda (x y)
	    (polygon (mapcar (lambda (xx yy)
			       (calc-pt (cons (+ x (* xx *dot-size*)) ;; scale fitness 
					      (+ y (* yy *dot-size*)))))
			     '(-1 -1 1 1)	;; size of dice : x
			     '(-1 1 1 -1))	;; size of dice : y
		     '(255 255 255)))
	  '(-0.05 0.125 0.3 -0.3 -0.125 0.05 0.2 0.2 0.45 0.45 -0.45 -0.2) ;; dice's pos : x
	  '(-0.875 -0.80 -0.725 -0.775 -0.70 -0.625 -0.35 -0.05 -0.45 -0.15 -0.45 -0.05)))) ;; dice's pos : y
	;; it has twelve element ? -> 2 4 6 of dice-number

(defun draw-tile-svg (x y pos hex xx yy col chosen-tile)
  (loop for z below 2	;; two object : six vertix
       do (polygon (mapcar (lambda (pt)
			     (cons (+ xx (* *board-scale* (car pt)))
				   (+ yy (* *board-scale* (+ (cdr pt) (* (- 1 z) 0.1)))))) ;; (- 1 z)
			   '((-1 . -0.2) (0 . -0.5) (1 . -0.2) ;; six vertix
			     (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
		   (if (eql pos chosen-tile) ;; highligth
		       (brightness col 100)
		       col)))
  (loop for z below (second hex) ;; dice num
       do (draw-die-svg (+ xx (* *dice-scale*  ;; 0.9 or -0.9 
				 0.3
				 (if (oddp (+ x y z))
				     -0.3
				     0.3)))
			(- yy (* *dice-scale* z 0.8)) col)))   

(defparameter *die-colors* '((255 63 63) (63 63 255)))

(defun draw-board-svg (board chosen-tile legal-tiles)
  (loop for y below *board-size*
       do (loop  for x below *board-size*
	       for pos = (+ x (* *board-size* y))
	       for hex = (aref board pos)
	       for xx  = (* *board-scale* (+ (* 2 x) (- *board-size* y)))
	       for yy  = (* *board-scale* (+ (* 0.7 y) *top-offset*))
	       for col = (brightness (nth (first hex)  *die-colors*)
				     (* -15 (- *board-size* y)))
	       do (if (or (member pos legal-tiles) (eql pos chosen-tile))
		      (tag g ()
			(tag a ("xlink:href" (make-game-link pos)) ;; link to the pos
			  (draw-tile-svg x y pos hex xx yy col chosen-tile)))
		      (draw-tile-svg x y pos hex xx yy col chosen-tile)))))

(defun make-game-link (pos)
  (format nil "/game.html?chosen=~a" pos))

(defparameter *cur-game-tree* nil)
(defparameter *from-tile* nil)

(defun dod-request-handler (path header params)
  (if (equal path "game.html") ;; access currect page?
      (progn (princ "HTTP/1.1 200 OK")
	     (terpri)
	     (terpri)
	     (princ "<!doctype html>") ;; this page is html
	     (tag center ()	;; <center> ... </center>
	       (princ "Welcome to DICE OF DOOM!")
	       (tag br ())
	       (let ((chosen (assoc 'chosen params))) ;; params include chosen-pos 
		 (when (or (not *cur-game-tree*) (not chosen)) ;; player not select / game is not found
		   (setf chosen nil)			       ;; initialize
		   (web-initialize))				
		 (cond ((lazy-null (caddr *cur-game-tree*))	;; no moves
			(web-announce-winner (cadr *cur-game-tree*))) ;; from board
		       ((zerop (car *cur-game-tree*)) ;; you are the player
			(web-handle-human	     
			 (when chosen		      ;; selected pos
			   (read-from-string (cdr chosen)))))	;; chosen's pos-number
		       (t (web-handle-computer)))) ;; AI is the player
	       (tag br ())
	       (draw-dod-page *cur-game-tree* *from-tile*)))
      (princ "Sorry... I don't know that page.")))

;;(defun hello-request-handler (path header params)
;;  (declare (ignore header))
;;  (if (equalp path "greeting")	
;;      (let ((name (assoc 'name params)))
;;	(if (not name)
;;	    (progn
;;              (princ "HTTP/1.1 200 OK")
;;              (terpri)
;;              (terpri)
;;	      (princ "<html><form>What is your name?<input name = 'name' /></form></html>"))
;;	    (progn
;;              (princ "HTTP/1.1 200 OK")
;;              (terpri)
;;              (terpri)
;;	    (format t "<html>Nice to meet you, ~a!</html>" (cdr name)))))
;;      (princ "Sorry... I don't know that page")))

(defun web-initialize ()
  (setf *from-tile* nil)
  (setf *cur-game-tree* (game-tree (gen-board) 0 0 t)))

(defun web-announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
	(format t "The game is a tie between ~a" (mapcar #'player-letter w))
	(format t "The winner is ~a" (player-letter (car w)))))
  (tag a (href "game.html") ;; reset button
    (princ "play-again")))

(defun web-handle-human (pos)
  (cond ((not pos) (princ "Please choose a hex to move from: "))
	;; pass
	((eq pos 'pass) (setf *cur-game-tree*  ;; change to AI player = go into next nest
			      (cadr (lazy-car (caddr *cur-game-tree*))))
	 (princ "Your reinforcements have been placed.") ;; reinforcements are automatically
	 (tag a (href (make-game-link nil))
	   (princ "continue")))
	;; had selected nothing
	((not *from-tile*) (setf *from-tile* pos)
	 (princ "Now choose a destination: "))
	;; selected same pos -> sign of cancel
	((eq pos *from-tile*) (setf *from-tile* nil)
	 (princ "Move cancelled."))
	;; others -> move anything
	(t (setf *cur-game-tree*
		 (cadr (lazy-find-if (lambda (move) ;; ex. ((1 2) (GAME-TREE)) ;; return GAME-TREE
				       (equal (car move) ;; ex. (1 2)
					      (list *from-tile* pos))) ;; ex. (2 3)
				     (caddr *cur-game-tree*)))) ;; moves list
	   (setf *from-tile* nil)	;; reset selected-pos
	   (princ "You may now ")	;; pass or select another move
	   (tag a (href (make-game-link 'pass))
	     (princ "pass")) 
	   (princ " or make another move: "))))

(defun web-handle-computer ()
  (setf *cur-game-tree* (handle-computer *cur-game-tree*))
  (princ "The computer has moved. ")
  (tag script ()
    (princ "window.setTimeout('window.location=\"game.html?chosen=NIL\"',500)"))) ;; reload page

(defun draw-dod-page (tree selected-tile)
  (svg *board-width*
       *board-height*
       (draw-board-svg (cadr tree)
		       selected-tile
		       (take-all (if selected-tile ;; condition
				     (lazy-mapcar ;; return result's list ;; true
				      (lambda (move) ;; ex. (1 2) (1 3) (2 3) ...
					(when (eql (caar move)
						   selected-tile)
					  (cadar move))) ;; ex. 1 1 2 ...
				      (caddr tree)) ;; moves
				     (lazy-mapcar #'caar (caddr tree))))))) ;; all moves ;; false
			    
;; (serve #'dod-request-handler)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; svg let us graphical game
;; html5 permit us using svg image
;; you should expand some ability : some players' game (use request handler)
