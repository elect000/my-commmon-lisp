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
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num*
		       collect (edge-pair (random-node) (random-node)))))

;; make edges for *edge-num* times
;; these edges are pair edges

(loop repeat 10
     collect 1)

;; (1 1 1 1 1 1 1 1 1 1)

(loop repeat 3
     collect (list 'a 'b))

;; (a b a b a b)

(loop for n from 1 to 5
     collect (+ 100 n))

;; (101 102 103 104 105)

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x)
		   (eql (car x) node))
		 edge-list))

;; -> ((node some1) (node some2) (node some3) ...)

(defun get-connected (node edge-list) 	;; node = start point ex. 'key / edge-list = "all" edges 
  (let ((visited nil)) 			;; nil initialize
    (labels ((traverse (node)	       
	       (unless (member node visited)
		 (push node visited) 	;; first initialize with node
		 (mapc (lambda (edge)	;; dot list      
			 (traverse (cdr edge))) 
		       (direct-edges node edge-list)))))
      (traverse node))
    visited))

;; (get-connected 'key '((c . extra) (key . a) (a . b) (key . c) (e . f) (c . d) (c . e)))
;; -> (f e d extra c b a key) ... connected in find-islands (below)

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
;; ((a . d) (d . a) (d . a) (a . d) (k . something) ...)

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))


(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num*
		     collect i))
	 (edge-list (connect-all-islands nodes (make-edge-list)))
	 (cops (remove-if-not (lambda (x)
				(declare (ignore x)) ;;?
				(zerop (random *cop-odds*)))
			      edge-list)))
    (add-cops (edges-to-alist edge-list) cops)))

(defun edges-to-alist (edge-list)	
  (mapcar (lambda (node1)
	    (cons node1
		  (mapcar (lambda (edge)
			    (list (cdr edge)))
			  (remove-duplicates (direct-edges node1 edge-list)
					     :test #'equal))))
	  (remove-duplicates (mapcar #'car edge-list))))

(defun add-cops (edge-alist edges-with-cops)
  (mapcar (lambda (x)
	    (let ((node1 (car x))
		  (node1-edges (cdr x)))
	      (cons node1 
		    (mapcar (lambda (edge)
			      (let ((node2 (car edge)))
				(if (intersection (edge-pair node1 node2)
						  edges-with-cops
						  :test #'equal)
				    (list node2 'cops)
				    edge)))
			    node1-edges))))
	  edge-alist))
