;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; loop/format ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(format t "Add onion rings for only ~$ dollers more!" 1.5)

;; about format ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~$ is manage sequence					;;
;; t  is output space (t = console / nil = str /stream stream)	;;
;; argument 							;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; string ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~s == prin1
(prin1 "foo") ;; "foo"
(format t "I am printing ~s in the middle of this sentence." "foo")

;; ~a
(princ "foo") ;; foo
(format t "I am printing ~a in the middle of this sentence." "foo")

;; length of sentence ... foo_______
(format t "I am printing ~10a within ten space of room." "foo")

;; length of sentence ... _______foo
(format t "I am printing ~10@a within ten space of room." "foo")

;; length of sentence ... foo_________ == add every 3 spaces to over 10 charactors ex. 9 -> 12 -> 15
(format t "I am printing ~10,3a within ten space of room." "foo")

;; add 4 spaces
(format t "I am printing ~,,4a in the middle of this sentence." "foo") ;; four space

;; add 4 !
(format t "I am printing ~,,4,'!a in the middle of this sentence." "foo") ;; four !

(format t "I am printing ~,,4,'!@a in the middle of this sentence." "foo") 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; number ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ~x == 16 Decimal number
(format t "The number 1000 in hexadecimal is ~x" 1000) ;; 3E8

;; ~b == Decimal number
(format t "The number 1000 in decimal is ~b" 1000) ;; 11111010000

;; ~d == 10 decimal number
(format t "The number 1000 in decimal is ~d" 1000) ;; 1000

;; group number
(format t "Number with commas in them are ~:d times better." 1000000) ;; 1,000,000

;; binding parameter
(format t "I am printing ~10d within ten spaces of room." 1000000) ;; ___1000000

(format t "I am printing ~10,'xd within ten spaces of room." 1000000) ;; xxx1000000

;; float
(format t "PI can be estimated as ~4f" 3.141592) ;; 3.14

;; 4 number after .
(format t "PI can be estimated as ~,4f" 3.141592) ;; 3.1416

(format t "PI can be estimated as ~,4f" PI) ;; 3.1416

;; *10
(format t "Percentages are ~,,2f percent better than fractions" 0.77) ;; 77.0

;; price
(format t "I wish I had ~$ dollers in my bank account." 1000000.2) 


;; terpri == #\return
(progn (princ 22)
       (terpri)
       (princ 33))

;; fresh-line
(progn (princ 22)
       (fresh-line)
       (fresh-line)
       (princ 33))

;; ~% ... tepri
;; ~& ... fresh-line
(progn (format t "this is on one line ~%")
       (format t "~%this is on another line"))

;; this is on one line
;;
;; this in on another line

(progn (format t "this is on one line ~&")
       (format t "~&this is on another line"))

;; this is on one line
;; this is on another line

(format t "this will print ~5%on two lines spread far apart")

;; set text
(defun random-animal ()
  (nth (random 5) '("dog" "tick" "tiger" "walrus" "kangaroo")))

;; ~t is column position 
(loop repeat 10
     do (format t "~5t~a ~15t~a ~25t~a~%"
		(random-animal) ;; _____some______some______some
		(random-animal) ;;      5         15        25
		(random-animal))) 

;; auto indent
(loop repeat 10
     do (format t "~30<~a~;~a~;~a~>~%"
		 (random-animal)
		 (random-animal)
		 (random-animal)))

;; ~30> .. all space and start sequence
;; ~a  ... sentence
;; ~;  ... make new start point
;; ~>  ... end sequence
;; ~%  ... newline

;; auto indent with centering
(loop repeat 10 do (format t "~30:@<~a~>~%"
			   (random-animal)))

(loop repeat 10 do (format t "~30:@<~a~;~a~;~a~>~%"
			   (random-animal)
			   (random-animal)
			   (random-animal)))

;; auto indent with centering by one line
(loop repeat 10 do (format t "~10:@<~a~>~10:@<~a~>~10:@<~a~>~%"
			   (random-animal)
			   (random-animal)
			   (random-animal)))

(defparameter *animals* (loop repeat 10 collect (random-animal)))

(format t "~{I see a ~a! ~}" *animals*)

(format t "~{I see a ~a ... or was it a ~a?~%~}" *animals*)

(format t "|~{~<|~%|~,33:;~2d ~>~}|" (loop for x below 100 collect x))

;; if print 33 charactor , ~%
;; number of list print as 2 charactor ex. 2 -> _2
;; space of ~2d_ is means one space
;; A ~num1,num2:; B ... if sentence length is over num2-num1 -> A / else B


(defun robots ()
  (loop named main
       with directions = '((q . -65) (w . -64) (e . -63) (a . -1)
			   (d . 1) (z . 63) (x . 64) (c . 65))
       for pos = 544
       then (progn (format t "~%qwe/asd/zxc to move, (t)eleport, (l)eave: ")
		   (force-output)
		   (let* ((c (read))
			  (d (assoc c directions)))
		     (cond (d (+ pos (cdr d)))
			   ((eq 't c) (random 1024))
			   ((eq 'l c) (return-from main 'bye)) ;; exit and return 'bye
			   (t pos))))
       for monsters = (loop repeat 10
			   collect (random 1024))
       then (loop for mpos in monsters
		 collect (if (> (count mpos monsters) 1)
			     mpos
			     (cdar (sort (loop for (k . e) in directions
					      for new-mpos = (+ mpos e)
					      collect (cons (+ (abs (- (mod new-mpos 64)
								       (mod pos 64)))
							       (abs (- (ash new-mpos -6)
								       (ash pos -6))))
							    new-mpos))
					 '<
					 :key #'car))))
       when (loop for mpos in monsters
		 always (> (count mpos monsters) 1))
       return 'player-wins
       do (format t
		  "~%|~{~<|~%|~,65:;~A~>~}|"
		  (loop for p below 1024
		       collect (cond ((member p monsters)
				      (cond ((= p pos) (return-from main 'player-loses))
					    ((> (count p monsters) 1) #\#)
					    (t #\A)))
				     ((= p pos)
				      #\@)
				     (t
				      #\ ))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; format / first argument ... output to repl or stirng
;; format / second argument ... manage sentence
;; format / others         ... value
;; string : ~a ~s
;; number : ~d ~$ ~b ~x ~4f ~,4f ~,,2f etc ...
;; format has a lot of func such as loop ...
 
