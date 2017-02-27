;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; loop/format ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop 	for i
   	below 5
   	sum i)
;; (+ 1 2 3 4)

(loop for i
   from 5
   to 10
   sum i)

;; -> 45

(loop for i
     in '(100 20 3)
     sum i)

;; -> 123

(loop for i
     below 5
     do (print i))

;; 0 #\newline 1 ... #\newline 4

(loop for i
     below 10
     when (oddp i)
     sum i)

;; 25 = (+ 1 3 5 7 9)

(loop for i
     from 0
     do (print i)
     when (= i 5)
     return 'falafel)

;; 0 #\newline 1 #\newline ... 5 #\newline FALAFEL

(loop for i
     in '(2 3 4 5 6)
     collect (* i i))

;; (4 9 16 25 36)

(loop for x below 10
     for y below 10
     collect (+ x y))

;; (0 2 4 8 16 18)

(loop for x below 10
     collect (loop for y below 10
		  collect (+ x y)))

;;((0 1 2 3 4 5 6 7 8 9) (1 2 3 4 5 6 7 8 9 10) (2 3 4 5 6 7 8 9 10 11)
;; (3 4 5 6 7 8 9 10 11 12) (4 5 6 7 8 9 10 11 12 13) (5 6 7 8 9 10 11 12 13 14)
;; (6 7 8 9 10 11 12 13 14 15) (7 8 9 10 11 12 13 14 15 16)
;;  (8 9 10 11 12 13 14 15 16 17) (9 10 11 12 13 14 15 16 17 18))

(loop for i
     from 0
     for day
     in '(monday tuesday wednesday thursday friday saturday sunday)
     collect (cons i day))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loop macro table ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; simple
;;(loop
;;     (princ "type something: ")
;;     (force-output)
;;     (read))

;; do/ing
(loop for i below 5
     do (print i)) ;; 0 <= i <= 4

;; repeat 
(loop repeat 5
     do (print "Prints five times"))

;; initially
(loop initially
     (print 'loop-begin)
     for x below 3
     do (print x)) ;; loop-begin #\n 0 ...2

;; finally
(loop for x below 3
     do (print x)
     finally
     (print 'loop-end)) ;; 0 #\n ...2 #\n loop-end

;; named & return-from 
(loop named outer
     for i below 10 ;; 0 <= i <= 9
     do
     (progn
       (print "outer")
       (loop named inner
	    for x below i ;; 0 <= x <= i 
	    do
	    (print "**innner")
	    when (= x 2) ;; exit from outer!
	    do
	    (return-from outer
	      'kicked-out-all-the-way))))

;; while 
(loop for i in '(0 2 4 555 6)
     while (evenp i) ;; if odd = i => exit 
     do (print i))

;; until
(loop for i 	    ;; -> 0 ...4
     from 0
     do (print i)   
     until (> i 3)) ;; check is started after print

;; using : hash-table :
(defparameter salary (make-hash-table))
(setf (gethash 'bob salary) 80)
(setf (gethash 'john salary) 70)
(loop for person being each hash-key of salary using (hash-value amt)
     doing (print (cons person amt))) ;; ex. (BOB . 80)

;; being : hash-table :
(loop for person being each hash-key of salary
     doing (print person))

;; with : local variable
(loop with x = (+ 1 2)
     repeat 5
     do (print x)) ;; -> 3 #\n ... 3

;; the == each
(loop for person being the hash-key of salary do (print person))

;; each
(loop for person being each hash-key of salary do (print person)) ;; ex. BOB

;; hash-keys == hash-key
(loop for person being the hash-keys of salary do (print person))

;; hash-key
(loop for person being the hash-key of salary do (print person))

;; hash-values == hash-value
(loop for amt being the hash-value of salary do (print amt)) ;; ex. 80

;; hash-value
(loop for amt being the hash-values of salary do (print amt))

;; for == as
(loop for i
     from 0
     do (print i)
     when (= i 5)
     return
     'zuchini)

;; as
(loop as x
     from 0
     to 10
     collect x)

;; in 
(loop for i
     in '(1 2 3 4)
     do (print i)) ;; -> 1 2 3 4

;; on
(loop for x
     on '(1 2 3 4)
     do (print x)) ;; -> (1 2 3 4) (2 3 4) (3 4) (4)

;; across : array :
(loop for i
     across
     #(100 20 3)
     sum i) ;; 123

;; by 
(loop for i
     from 6
     to 8 by 2
     sum i) ;; -> 6 + 8 = 14

;; from 
(loop for i 
     from 6
     to 8 
     sum i) ;; 21

;; to 
(loop for i
     from 6
     to 8
     sum i) ;; 21

;; upfrom
(loop for i
     upfrom 6
     to 8
     sum i)

;; downfrom
(loop for i
     downfrom 8
     to 6
     sum i)

;; upto
(loop for i
     from 6
     upto 8
     sum i)

;; downto
(loop for i
     from 8
     downto 6
     sum i)

;; then
(loop repeat 5
     for x = 10.0
     then (/ x 2)
     collect x) ;; (10.0 5.0 2.5 ...)

;; into : local variable
(loop for i
     in '(3 8 73 4 -5)
     minimize i
     into lowest
     maximize i
     into biggest
     finally
     (return (cons lowest biggest))) ;; (-5 . 73)

;; always
(loop for i
     in '(0 2 4 6)
     always (evenp i)) ;; T

;; never
(loop for i
     in '(0 2 4 6)
     never (not (evenp i))) ;; T

;; thereis
(loop for i
     in '(0 2 4 555 6)
     thereis (oddp i)) ;; T

;; if
(loop for i
     below 5
     if (oddp i)
     do (print i)) ;; 1 3

;; when
(loop for i
     below 4
     when (oddp i)
     do (print i)
     do (print "yup")) ;; "yup" 1 "yup" yup" 3 "yup"
		       ;;   0     1      2     3

;; unless
(loop for i below 4
     unless (oddp i)
     do (print i)) ;; 0 2

;; and
(loop for x below 5
     
   when (= x 3)
     do (print "do this") 
     and do (print "also do this")
     
     do (print "always do this"))

;; else
(loop for i
     below 5
     if (oddp i)
     do (print i)
     else
     do (print "w00t"))

;; end
(loop for i
     below 4
     when (oddp i)
     do (print i)
     end		;; it is called every times at the end
     do (print "yup")) 

;; count/ing
(loop for i
     in '(1 1 1 1)
     count i) ;; 4

;; sum/ing
(loop for i
     below 5
     summing i) ;; 10

;; minimize/ing
(loop for i in '(3 2 1 2 3)
     minimize i)

;; maximize/ing
(loop for i in '(1 2 3 2 1)
     maximizing i) ;; 3

;; append/ing
(loop for i 
     below 5
     appending (list 'Z i)) ;; (Z 0 Z 1 ... 4)

;; nconc/ing
(loop for i 
     below 5
     nconcing (list 'Z i)) ;; (Z 0 ...4)


;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nconc & append ;;;;;;;;;;;;;;;;;;;;;;
;; (defparameter foo '(a b))	      ;;
;; (defparameter bar '(c d))	      ;;
;;				      ;;
;; (append foo bar) 		      ;;
;; foo -> '(a b)		      ;;
;; bar -> '(c d)		      ;;
;;				      ;;
;; (nconc foo bar)		      ;;
;; foo -> '(a b c d) or others	      ;;
;; bar -> '(c d)     or others	      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; evaluating animals ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *width* 100)
(defparameter *height* 30)
(defparameter *jungle* '(45 10 10 10))
(defparameter *plant-energy* 80)

(defparameter *plants* (make-hash-table :test #'equal)) ;; usually use #'eql
;; review eql -> symbol number character 
;; review equal -> neally all

(defun random-plant (left top width height)
  (let ((pos (cons (+ left (random width)) (+ top (random height)))))
    (setf (gethash pos *plants*) t))) ;; we don't think the value, so we only use "t"
;; review random A -> 0 <= return <= A-1

(defun add-plants ()		  ;; it create two plants
  (apply #'random-plant *jungle*) ;; *jungle* = '(A B C D) ... number's list
  (random-plant 0 0 *width* *height*))


;; abstract structure
(defstruct animal x y energy dir genes)  ;; have 5 elements

(defparameter *animals* 		   ;; this is a list, so it is first animal
  (list (make-animal :x (ash *width* -1)   ;; half of map
		     :y (ash *height* -1)  ;; half of map
		     :energy 100	   ;; life
		     :dir 0		   ;; initial direction
		     :genes (loop repeat 8 ;; this value is power/ not direction
				 collecting (1+ (random 10)))))) ;; element of genes: 1 <= x <= 10
;;;;;;;;;;;
;;  dir  ;;
;; 0 1 2 ;;
;; 7   3 ;;
;; 6 5 4 ;;
;;;;;;;;;;;

(defun move (animal)
  (let ((dir (animal-dir animal))
	(x (animal-x animal))
	(y (animal-y animal)))
    (setf (animal-x animal) (mod (+ x
				    (cond ((and (>= dir 2) (< dir 5)) 1) ;; 2<= dir < 5 
					  ((or (= dir 1) (= dir 5)) 0)   ;; dir = 1 or 5
					  (t -1)))
				 *width*)) ;; mod a b .. a < b/ if move max -> move to min 
    (setf (animal-y animal) (mod (+ y
				    (cond ((and (>= dir 0) (< dir 3)) -1)
					  ((and (= dir 7) (= dir 3)) 0)
					   (t 1)))
				 *height*))
    (decf (animal-energy animal)))) ;; mod a b

(defun turn (animal)
  (let ((x (random (apply #'+ (animal-genes animal))))) ;; 0 <= x < sum '(animal-genes)
    (labels ((angle (genes x)				;; genes x
	       (let ((xnu (- x (car genes))))		;; loop start
		 (if (< xnu 0)
		     0					;; it has no meaning == damp data
		     (1+ (angle (cdr genes) xnu))))))	;; loop end
      (setf (animal-dir animal)				;; set direction
	    (mod (+ (animal-dir animal) (angle (animal-genes animal) x)) ;; add now state and xnu
		 8)))))					;; 0 <= dir < 7

(defun eat (animal)
  (let ((pos (cons (animal-x animal) (animal-y animal))))
    (when (gethash pos *plants*) ;; there is a plant
      (incf (animal-energy animal) *plant-energy*) 
      (remhash pos *plants*))))

;; remhash == remove hash
;; review :: incf A B ... A = A + B

(defparameter *reproduction-energy* 200)

(defun reproduce (animal)
  (let ((e (animal-energy animal)))
    (when (>= e *reproduction-energy*)
      (setf (animal-energy animal) (ash e -1)) ;; half of latest energy
      (let ((animal-nu 	(copy-structure animal))
	    (genes 	(copy-list (animal-genes animal)))
	    (mutation 	(random 8)))  ;; which slot of the gene list will change ?
	(setf (nth mutation genes) (max 1 (+ (nth mutation genes) (random 3) -1))) 
	(setf (animal-genes animal-nu) genes) ;; re-defun genes
	(push animal-nu *animals*)))))	      ;; add the list of animals

;; (nth mutation genes) + (-1 <= x <= 1) or 1 (minimam = 1)

(defun update-world ()
  (setf *animals* (remove-if (lambda (animal)
			       (<= (animal-energy animal) 0)) ;; killed enegy
			     *animals*))
  (mapc (lambda (animal) ;; mapc will not make list
	  (turn animal)
	  (move animal) ;; always 1 step move
	  (eat animal)
	  (reproduce animal))
	*animals*)
  (add-plants))

(defun draw-world ()
  (loop for y
       below *height*
       do (progn (fresh-line)
		 (princ "|")
		 (loop for x
		      below *width* ;; 0 <= animal, plant <= *width* - 1 
		      do (princ (cond ((some (lambda (animal)
					       (and 	(= (animal-x animal) x)
					       		(= (animal-y animal) y)))
					     *animals*)
				       #\M)
				      ((gethash (cons x y) *plants*)
				       #\*)
				      (t #\space))))
		 (princ "|"))))

;; review: t -> cond
;; review: otherwise -> case

(defun evolution ()
  (draw-world)
  (fresh-line)
  (let ((str (read-line)))
    (cond ((equal str "quit") ())
	  (t (let ((x (parse-integer str :junk-allowed t)))
	       (if x
		   (loop for i
			below x
			do (update-world)
			if (zerop (mod i 1000)) ;; if 1000 times passed, print "." 
			do (princ #\.))
		   (update-world))
	       (evolution))))))

;; :junk-allowed ;; some -> nil / 1112nil -> 1112

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; how to use loop? (do you remember loop table)
