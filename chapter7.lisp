;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; over the list ;;;;;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; strange list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(cons 1 (cons 2 (cons 3 nil)))

;; dot list
(cons 1 (cons 2 3)) ;; -> ( 1 2 . 3)

'(1 .(2 .(3 . nil))) ;; -> (1 2 3)

(cons 2 3) ;; (2 . 3)

;; loop list
(setf *print-circle* t) ;; enablle to print circle

(defparameter foo (list 1 2 3))

(setf (cdddr foo) foo) ;; #1=(1 2 3 . #1#)

;; association list (alist)
(defparameter *drink-order* '((bill . double-espresso)
			      (lisa . small-drip-coffee)
			      (john . medium-latte)))

(assoc 'lisa *drink-order*)

(push '(lisa . large-mocha-with-whipped-cream) *drink-order*)

;; complement data
(defparameter *house* '((walls (mortar 	(cement)
					(water)
					(sand))
			 	(bricks))
			(windows(glass)
			 	(frame)
			 	(curtains))
			(roof	(shingles)
			 	(chimney))))

(defparameter *wizard-nodes* '((living-room (you are in the living-room.
					     a wizard is snoring loudly on the couch.))
			       (garden 	    (you are in a beautiful garden.
					     there is a well in front of you.))
			       (attic (you are in the attic. 
				       there is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
					    (attic upstairs ladder))
			       (garden 	    (living-room east door))
			       (attic  	    (living-room downstairs ladder))))

;; change_name
(defun dot-name (exp)
  (substitute-if #\_(complement #'alphanumericp) (prin1-to-string exp)))

;; change not(alphabet and number) to _(#\_) in exp
(substitute-if #\e #'digit-char-p "I'm  a l33t hack3r!")

(substitute-if 0 #'oddp '(1 2 3 4 5 6 7 8))

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil))) ;; pretty has #\tab or #\newline. remove them.
	(if (> (length s) *max-label-length*) ;; too-large length
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...") 
	    ;; set the sentence size & change to string
	    s)) ;; others
      "")) ;; no name

(dot-label 'it-can-set-the-lebel-name-following-the-rule) ;;"it-can-set-the-label-name-f..."

(defun nodes->dot (nodes)
  (mapc (lambda (node) ;; mapc... pick-up node in nodes 
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
  nodes))
;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mapcar? mapc? ;;;;;;;;;;;;;;;;;;;;;;;;;
;; mapcar: return changed list ; slow   ;;
;; mapc  : return nothing ;;;;;; faster ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun edges->dot (edges)
  (mapc (lambda (node)
	  (mapc (lambda (edge)
		  (fresh-line)
		  (princ (dot-name (car node)))
		  (princ "->")
		  (princ (dot-name (car edge)))
		  (princ "[label=\"")
		  (princ (dot-label (cdr edge)))
		  (princ "\"];"))
		(cdr node)))
	edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output* 	;; *standard-output* is usually console 
		   fname	     	;; so cahnge it to output file
		   :direction :output
		   :if-exists :supersede)
    (funcall thunk))                    ;; in thunk function (no-argument function = nullary function)
    (sb-ext:run-program "/bin/sh"
                      (list "-c" (concatenate 'string "dot -Tpng -O " fname))))

;; file stream
(with-open-file (my-stream
		 "testfile.txt"
		 :direction :output
		 :if-exists :supersede)
  (princ "Hello, File!" my-stream))

;;;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; keywaord symbol					       	     ;;;;
;; :cigar							     ;;;;
;;								     ;;;;
;; (let ((:cigar))						     ;;;;
;;	:cigar)							     ;;;;
;; -> :cigar is a keyword, and cannot be used as a local variable.   ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda () ;; no-argument
	      (graph->dot nodes edges)))) ;; eval later

(graph->png "wizard.dot" *wizard-nodes* *wizard-edges*)

;; thunk is for late function

(defun uedges->dot (edges)
  (maplist (lambda (lst) ;;ex. ((living-room (garden west door) ()) (garden ()) (attic ()))
	     (mapc (lambda (edge) ;; ex. (garden west door)
		     (unless (assoc (car edge) (cdr lst)) ;; ex. (garden ...) & ((garden ()) (attic ()))
		       (fresh-line)			  ;; assoc read 1st hierarchy 
		       (princ (dot-name (caar lst)))
		       (princ "--")
		       (princ (dot-name (car edge)))
		       (princ "[label=\"")
		       (princ (dot-label (cdr edge)))
		       (princ "\"];")))
		   (cdar lst)))
	   edges))

;; maplist ... return list 

(mapcar #'print '(a b c)) ;; -> A #\newline B #\newline C
(maplist #'print '(a b c));; -> (A B C) #\newline (B C) #\newline (C)

;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; assoc read 1st hierarchy			  ;;
;; (assoc 'key '((a (key other)) (b (key other))));;
;; -> NIL					  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))

(ugraph->png "u.wizard.dot" *wizard-nodes* *wizard-edges*)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; dot list
;; pair is dot list which has two elements
;; loop list
;; associate list ... key and value
;; how to use graphviz
;; lisp must use console, do not use file 
;;    output file from console with thunk

 
