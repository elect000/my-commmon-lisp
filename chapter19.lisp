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
  (loop for z below 2
       do (polygon (mapcar (lambda (pt)
			     (cons (+ xx (* *board-scale* (car pt)))
				   (+ yy (* *board-scale* (+ (cdr pt) (* (- 1 z) 0.1))))))
			   '((-1 . -0.2) (0 . -0.5) (1 . -0.2)
			     (1 . 0.2) (0 . 0.5) (-1 . 0.2)))
		   (if (eql pos chosen-tile)
		       (brightness col 100)
		       col)))
  (loop for z below (second hex)
       do (draw-die-svg (+ xx (* *dice-scale*
				 0.3
				 (if (oddp (+ x y z))
				     -0.3
				     0.3)))
			(- yy (* *dice-scale* z 0.8)) col)))  
