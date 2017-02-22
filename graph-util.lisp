;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; graph-util.lisp ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dot-name (exp)
  (substitute-if #\_(complement #'alphanumericp) (prin1-to-string exp)))


(defparameter *max-label-length* 30)
(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil))) ;; pretty has #\tab or #\newline. remove them.
	(if (> (length s) *max-label-length*) ;; too-large length
	    (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...") 
	    ;; set the sentence size & change to string
	    s)) ;; others
      "")) ;; no name

(defun nodes->dot (nodes)
  (mapc (lambda (node) ;; mapc... pick-up node in nodes 
	  (fresh-line)
	  (princ (dot-name (car node)))
	  (princ "[label=\"")
	  (princ (dot-label node))
	  (princ "\"];"))
  nodes))

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

(defun graph->png (fname nodes edges)
  (dot->png fname
	    (lambda () ;; no-argument
	      (graph->dot nodes edges)))) ;; eval later

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

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
	    (lambda ()
	      (ugraph->dot nodes edges))))

