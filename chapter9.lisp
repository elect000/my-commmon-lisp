;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; ork battle ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; how to use array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-array 3)
;; -> #(0 0 0)

(make-array 3 :initial-element nil)
;; -> #(MIL NIL NIL)

(defparameter x (make-array 3))
;; -> #(0 0 0)

(setf (aref x 1) 'foo)
;; #(0 FOO 0)

(aref x 1)
;; FOO

(setf foo (list 'a 'b 'c))
;; -> (A B C)

(second foo)
;; -> B

(setf (second foo) 'z)
;; -> (A Z C)

(setf foo (make-array 4 :initial-element nil))
;; -> (NIL NIL NIL NIL)

(setf (aref foo 2) (list 'x 'y 'z))
;; -> (NIL NIL (X Y Z) NIL)

(setf (car (aref foo 2)) (make-hash-table))

(setf (gethash 'zoink (car (aref foo 2))) 5)
;; (NIL NIL (S#(HASH-TABLE (ZOINK . 5)) Y Z) NIL)

;; how to use list ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(nth 1 '(foo bar baz))
;; bar

;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; speed of array or list                         ;;
;; array can access too far date faster           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; how to use hashtable ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(make-hash-table)

(defparameter x (make-hash-table))

(gethash 'yup x) 
;; -> NIL ... key number
;;    NIL ... not exist  
;; because there is nothing in the hash-table

(defparameter x (make-hash-table))

(setf (gethash 'yup x) '25) ;; set

(gethash 'yup x) 
;; -> 25 ... key number 
;;    T  ... exist

(defparameter *drink-order* (make-hash-table))

(setf (gethash 'bill *drink-order*) 'double-espresso)

(setf (gethash 'lisa *drink-order*) 'small-drip-coffee)

(setf (gethash 'john *drink-order*) 'medium-latte)

(gethash 'lisa *drink-order*)

;; some functions return some value ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(round  2.4)
;; -> 2
;;    0.4

(defun foo ()
  (values 3 7))
;; -> 3
;;    7

(+ (foo) 5)
;; 8
;; use first value

(multiple-value-bind (a b) (foo)
  (* a b))
;; a = 3
;; b = 7
;; -> 21

(load "chapter8")



;; override
(defun hash-edges (edge-list) ;; ex ((1 . 2) (3 . 4) ...)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
	    (let ((node (car x)))
	      (push (cdr x) (gethash node tab))))
	  edge-list)
    tab))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table))) 		;; initialize
    (labels ((traverse (node)			;; from node to anywhere
	       (unless (gethash node visited)	;; no entry in the hash table = not visiting
		 (setf (gethash node visited) t) ;; add entry t in the key
		 (mapc (lambda (edge)		
			 (traverse edge))	   
		       (gethash node edge-tab))))) ;; search all edges from the node 
      (traverse node))
    visited)) ;; return 

;;(time (dotimes (i 500000) (get-connected 1 (make-edge-list))))
;; nearly 13 seconds

;;(time (dotimes (i 500000) (get-connected-hash 1 (hash-edges (make-edge-list))))) 				
;; nearly 8 seconds

;; data structure ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct 	person
  		name
		age
		waist-size
		favorite-color)

(defparameter *bob* (make-person :name "Bob"
				 :age 35
				 :waist-size 32
				 :favorite-color "blue"))

;; make-,(data-structure) is defined automatically
;; -> (PERSON :NAME "Bob" :AGE 35 ...)

(person-age *bob*)

;; -> 35
;; ,(data-structure)-slot is defined automatically

(setf (person-age *bob*) 36)


;; (defparameter *THAT-GUY* #S(PERSON :NAME "Bob" :AGE 36 :WAIST-SIZE 32 :FAVORITE-COLOR "blue")) 
;; -> it is active in repl buffer so this cannot compile

(defun make-person (name age waist-size favorite-color)
  (list name age waist-size favorite-color))

(defun person-age (person)
  (cadr person))

(defparameter *bob* (make-person "bob" 35 32 "blue"))

(person-age *bob*)
;; -> 35

;; genelic function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sequence function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example
(length '(a b c)) ;; -> 3 / or (list-length '(a b c))

(length "blue")	  ;; -> 4

(length (make-array 5)) ;; -> 5

(find-if #'numberp '(a b 5 d)) ;; -> 5

(count #\s "mississippi") ;; -> 4

(position #\4 "2kewl4skewl") ;; -> 5

(some #'numberp '(a b 5 d)) ;; -> T

(every #'numberp '(a b 5 d)) ;; -> NIL

;; reduce
(reduce #'+ '(1 2 3 4)) ;; -> 10

(reduce #'cons '(a b c d)) ;; -> (((A . B) . C) . D)

(reduce (lambda (best item) 
	  (if (and (evenp item) (> item best))
	      item
	      best))
	'(7 4 6 5 2)
	:initial-value 0)

;; -> 6
;; 1st argument(best) <= initial-value / 2nd argument(item) <= 7

(reduce (lambda (best item) 
	  (if (and (evenp item) (> item best))
	      item
	      best))
	'(7 4 6 5 2))	

;; -> 7
;; 1st argument(best) <= 7 / 2nd argument(item) <= 4

(defun sum (lst)
  (reduce #'+ lst))

(sum '(1 2 3)) ;; -> 6

(sum (make-array 5 :initial-contents '(1 2 3 4 5))) ;; -> 15

;; (sum "blablabla") ;; -> error

(map 'list
     (lambda (x)
       (if (eq x #\s)
	   #\S
	   x))
     "this is a string")

;; -> (#\t #\h #\i #\S #\  #\i #\S #\  #\a #\  #\S #\t #\r #\i #\n #\g)

(map 'string
     (lambda (x)
       (if (eq x #\s)
	   #\S
	   x))
     "this is a string")

;; -> "thiS iS a String"


