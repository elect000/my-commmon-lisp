;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; grand seft onpus ;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "graph-util")

(defparameter *congestion-city-nodes* nil)

(defparameter *congestion-city-edges* nil)

(defparameter *visited-nodes* nil)

(defparameter *node-num* 30)

(defparameter *edge-num* 45)

(defparameter *worm-num* 3)

(defparameter *cop-odds* 15)

(defun random-node ()
  (1+ (random *node-num*))) 

;; (random num) -> 0 < value < num (random)
;; 1+ ?         -> 1 =< node-number =< num

(defun edge-pair (a b) ;; make dot list !!!
  (unless  (eql a b)
    (list (cons a b) (cons b a)))) ;; dot list

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
		       collect (edge-pair (random-node) (random-node)))))

;; make edges for *edge-num* times
;; these edges are pair edges

;; how to use loop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(loop repeat 10
     collect 1)

;; (1 1 1 1 1 1 1 1 1 1)

(loop repeat 3
     collect (list 'a 'b))

;; (a b a b a b)

(loop for n from 1 to 5
     collect (+ 100 n))

;; (101 102 103 104 105)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))  ;; return T/f
		 edge-list))

;; -> ((node some1) (node some2) (node some3) ...)

(defun get-connected (node edge-list) 	;; node = start point ex. 'key / edge-list = "all" edges 
  (let ((visited nil)) 			;; nil initialize
    (labels ((traverse (node)	        ;; new function : traverse
	       (unless (member node visited)
		 (push node visited) 	;; add start point
		 (mapc (lambda (edge)	;; edge = dot list      
			 (traverse (cdr edge))) 
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))

;; (get-connected 'key '((c . extra) (key . a) (a . b) (key . c) (e . f) (c . d) (c . e)))
;; -> (f e d extra c b a key) ... connected by find-islands (below)

(defun find-islands (nodes edge-list)
  (let ((islands nil)) ;; initialize
    (labels ((find-island (nodes)
	       (let* ((connected (get-connected (car nodes) edge-list))
		      (unconnected (set-difference nodes connected)))
		 (push connected islands)
		 (when unconnected
		    (find-island unconnected)))))
      (find-island nodes))
    islands)) 

;; -> ((a b) (d e f g h) (a b c j k) ...) 
;; (b & d)/ (h & a) are unconnected  => will be connected by #'connect-with-bridges
;; (a & b & c & j & k) is connected

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caadr islands)) ;; 1st element in 1st lists & 1st element in 2nd lists
	    (connect-with-bridges (cdr islands)))))    ;; 2nd lists & 3rd lists / 3rd lists & 4th lists /...

;; connect-with-bridges ( '((a b) (d e f g h) (a b c j k) ...) )
;; -> ((a . d) (d . a) (d . a) (a . d) (k . something) ...)

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

;; -> (list bridges islands'-edges)

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		     collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(declare (ignore x)) ;; don't use x in this func : x -> x
				(zerop (random *cop-odds*))) ;; is *cop-odds* zero? 
			      edge-list))) ;; if *odds-odds* is zero -> remaind in cops 
  (add-cops (edges-to-alist edge-list) cops)))

;; cops ex. ((15 . 17) (5 . 12) (13 . 11) ...)

(defun edges-to-alist (edge-list)	
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list) ;; !
					     :test #'equal)))) 
	  (remove-duplicates (mapcar #'car edge-list)))) ;; ((1. 2) (2. 3) (2. 1) (4. 1)) -> (1 (2) (4))

;; ! -> ((node1 some1) (node1 some2) (node1 some1) ...) -> ((node1 some1) (node1 some2) ...)
;; compare to list/remove-duplicates usually use eq
;; -> (node1 (some1) (some2) ....)
;; #check (edges-to-alist connect-all-islands nodes (make-edge-list))
;; #check (cons 1 (mapcar (lambda (x) (list x)) '(2 3 4))) -> (1 (2) (3) (4))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x) ;; ex. '(1 (2) (3) (4) ...)
	    (let ((node1 (car x))
		  (node1-edges (cdr x))) ;; '((2) (3) (4) ...)
	      (cons node1 
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops ;; (node1 . node2) or (node2 . node1) in it?
						  :test #'equal)
				    (list node2 'cops) ;; (node1 (node2)) -> (node1 (node2 'cops))
				    edge)))
			    node1-edges))))
	  edge-alist))

;; -> ((node1 (node2 'cops)) (node2 (node1 'cops) (node3)) (node3 (node2)))

(defun neighbors (node edge-alist)
  (mapcar #'car (cdr (assoc node edge-alist))))


;; (mapcar #'car (cdr (assoc 1 '((1 (2 cops) (3))))) -> (2 3)

(defun within-one (a b edge-alist)
  (member b (neighbors a edge-alist)))

(defun within-two (a b edge-alist)
  (or	(within-one a b edge-alist)
	(some (lambda (x)		;; x = one of results : (neighbors a edge-alist)
		(within-one x b edge-alist)) ;; is one of them T ? 
	      (neighbors a edge-alist))))

;; (some #'oddp '(2 4 5)) -> T
;; (all #'oddp '(2 4 5)) -> F

(defun make-city-nodes (edge-alist)
  (let	((wumpus (random-node))
	 (glow-worms (loop for i below *worm-num*
			  collect (random-node))))
    (loop for n from 1 to *node-num*
	 collect (append (list n)
			 (cond ((eql n wumpus) '(wumpus))
			       ((within-two n wumpus edge-alist) '(blood!)))
			 (cond ((member n glow-worms) '(glow-worms))
			       ((some (lambda (worm)
					(within-one n worm edge-alist))
				      glow-worms) ;; glow-worms is list ex.(1 2 3 4 5 ....)
				'(lights!)))
			 (when (some #'cdr (cdr (assoc n edge-alist))) 
			   '(sirens!))))))

;; (assoc 1 edge-alist) -> (node1 (node2) (node3 cops) ...)
;; cdr * -> ((node2) (node3 cops))
;; cdr * -> (node3 cops)
;; -> sirens!

;; !!! redefined !!!
;;(defun new-game ()
;;  (setf *congestion-city-edges* (make-city-edges))
;;  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
;;  (setf	*player-pos* (find-empty-node))
;;  (setf *visited-nodes* (list *player-pos*))
;;  (draw-city))

;; *congestion-city-edges* -> ex. ((1 (2 cops)) (2 (3) (4)) ...)
;; *congestion-city-nodes* -> ex. ((1 Blood!) (2 Blood sirens!) (3) ...)

(defun find-empty-node ()
  (let ((x (random-node))) 
    (if (cdr (assoc x *congestion-city-nodes*)) ;; ex (cdr (1 Blood!))	
	(find-empty-node) ;; retry
	x)))  		  ;; set pos

(defun draw-city ()
  (ugraph->png "city.dot" *congestion-city-nodes* *congestion-city-edges*))

(defun known-city-nodes ()
  (mapcar (lambda (node)
	    (if (member node *visited-nodes*) ;; ex. (member 4 '(2 3 5... ))
		(let ((n (assoc node *congestion-city-nodes*)))
		  (if (eql node *player-pos*) 
		      (append n '(*))
		      n)) ;; add nothing
		(list node '?))) ;; ex. (4 ?)
	  (remove-duplicates ;; remove same value in "a" list using eql  ex.(2 3 4... 5...)
	   (append *visited-nodes*		      ;; *visited-nodes* ex.(2 3 5... 2 3 4....)
		   (mapcan (lambda (node)	      ;; add next node / ! #'within-one !
			     (mapcar #'car            ;; (2)
				     (cdr (assoc node ;; node=1 : (1 (2 cops) ...) 
						 *congestion-city-edges*))))
			   *visited-nodes*)))))

;; (mapcan (lambda (x) (list x)) '(1 2 3 4 5)) -> (1 2 3 4 5)
;; (mapcar (lambda (x) (list x)) '(1 2 3 4 5)) -> ((1) (2) (3) (4) (5))

(defun known-city-edges ()
  (mapcar (lambda (node)
	    (cons node (mapcar (lambda (x)
			       (if (member (car x) *visited-nodes*)
				   x
				   (list (car x)))) ;; remove the keyword: cops / ex.(2 cops) -> (2)
			       (cdr (assoc node *congestion-city-edges*))))) ;; ex. ((2 cops) ...)
	  *visited-nodes*)) ;; ex. (1 2 3 4 5 ...)

(defun ingredients (order)
  (mapcan (lambda (burger)
	    (case burger
	      (single (list 'patty))
	      (double (list 'patty 'patty))
	      (double-cheese (list 'patty 'patty 'cheese))
	      ))
	  order))

;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mapcan want to get list                   ;;;;
;;   and return integrate list               ;;;;
;; ex. (ingredients '(single double-cheese)) ;;;;
;; -> ((patty) (patty patty cheese)) ,mapcar ;;;;
;; -> (patty patty patty cheese)     ,mapcan ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-known-city ()
  (ugraph->png "known-city.dot" (known-city-nodes) (known-city-edges)))

(defun new-game ()
  (setf *congestion-city-edges* (make-city-edges))
  (setf *congestion-city-nodes* (make-city-nodes *congestion-city-edges*))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*))
  (draw-city)
  (draw-known-city)
)

(defun walk (pos)
  (handle-direction pos nil)) 

(defun charge (pos)
  (handle-direction pos t))

(defun handle-direction (pos charging)				;; search the direction
  (let ((edge (assoc pos 					;; edge -> ((2 cops)) 
		     (cdr (assoc *player-pos* *congestion-city-edges*)))))
    (if edge  							;; is there ?
	(handle-new-place edge pos charging)			
	(princ "That location does not exit"))))

(defun handle-new-place (edge pos charging)			;; move selected place
  (let* ((node (assoc pos *congestion-city-nodes*))		;; node in the state where will be
	 (has-worn (and (member 'glow-worms node)		;; glow-worm is around there? 
			(not (member pos *visited-nodes*)))))	;; has there been not visited?  
    	;; => is there new worm? 
    (pushnew pos *visited-nodes*)				;; add pos if it isn't in *visited-nodes*
    (setf *player-pos* pos)					;; re-defun your place
    (draw-known-city)						;; new city map
    (cond ((member 'cops edge) 					;; cops are in edge ex. (1 (2 cops) (3) ...)
	   (progn (princ "You ran into the cops. Game Over ")
		  (new-game)))				       
	  ((member 'wumpus node) 				;; you can find the target 
	   (if charging						;; attacked?
	       (princ "You found the Wumpus! Game Clear! ")    
	       (progn (princ "You ran into the Wumpus. Wunpus runs away... Game Over")
		      (new-game))))
	  (charging (progn (princ "You wasted your last bullet. Game Over ") ;; couldn't killing 
			   (new-game)))
	  (has-worn (let ((new-pos (random-node)))		;; caught by gang : worm
		      (princ "You ran into a Glow Worn Gang! You're now at ")
		      (princ new-pos)				
		      (princ " ")
		      (handle-new-place nil new-pos nil))))))	;; force to move newplace (don't go through edge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; entrance in loop function
;; set-difference can search starange element in another list
;; intersection can search same element between some lists
;; remove-duplicate can remove same element in a list
;; #'truncate is return multiple values : (truncate 10 3) -> 3 1 => multiple-value bind
