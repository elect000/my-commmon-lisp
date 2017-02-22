;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; interpreter & file access ;;;;;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; print & prin1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(print "foo")

(progn (print "this")
       (print "is")
       (print "a")
       (print "test"))

;; "this"
;; "is" ...

(progn (prin1 "this")
       (prin1 "is")
       (prin1 "a")
       (prin1 "test"))

;; "this""is"...

(defun say-hello ()
  (prin1 "Please type your name:")
  (let ((name (read)))
    (prin1 "Nice to meet you, ")
    (prin1 name)))

;; Please type your name: ooo
;; Nice to meet you, ooo

(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "when I add five I get")
    (print (+ num 5))))

(print '3)   ;; -> 3
(print '3.4) ;; -> 3.4
(print 'foo) ;; -> FOO !big and small are same meanings
(print "foo") ;; -> "foo"
(print '#\a) ;; -> #\a

(print '|even this is a legal Lisp symbol!|) ;; -> |even this is a legal Lisp symbol!|

;; princ & read-line ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(princ '3) ;; -> 3
(princ '3.4) ;; -> 3.4
(princ 'foo) ;; -> FOO
(princ "foo");; -> foo
(princ '#\a) ;; -> a

(defun say-hello2 ()
  (princ "Please type your name:")
  (let ((name (read-line)))
    (princ "Nice to meet you: ")
    (princ name)))

;; read-line can read any charactor such as " " "!" etc...

;; differences bitween data and code ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

'(+ 1 2) ;; (+ 1 2)
(+ 1 2)  ;; 3

(defparameter *foo* '(+ 1 2))

(eval *foo*) ;; 3 ! data -> code ! 

;; defu my repl ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))

(defun game-read ()
  (let ((cmd (read-from-string ;; cmd is ...
	      (concatenate 'string "(" (read-line) ")")))) ;; (something something1 ...)
    (flet ((quote-it (x) ;; something1 ... are ...
		  (list 'quote x)))  
      (cons (car cmd) (mapcar #'quote-it (cdr cmd)))))) 
;; add quote = (quote something1) = 'something1 ...

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know such command.)))

(defun tweak-text (lst caps lit) ;; caps -> capslock / lit -> literature such as "LISP"
  (when lst
    (let ((item (car lst))
	  (rest (cdr lst)))
      (cond ((eql item #\space) (cons item (tweak-text rest caps lit))) ;; " " -> don't change
	    ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit))) ;; !/?/. -> capsup next char
	    ((eql item #\")  (tweak-text rest caps (not lit))) ;; " -> it is start of literature or end of it
	    (lit (cons item (tweak-text rest nil lit))) ;; literature -> don't activeate caps
	    (caps (cons (char-upcase item) (tweak-text rest nil lit)))  ;; caps up and reset caps
	    (t (cons (char-downcase item) (tweak-text rest nil nil))))))) ;; caps down (others)

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() " ;; remove first argument and ()
						  (prin1-to-string lst))
				     'list) ;; the argument is changed list
			     t
			     nil) ;; add argument
		 'string)) ;; change to string
  (fresh-line)) ;; line fresh

(game-print '(This is a sentence. What about this? probably.))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; print & read
;; princ & read-line
;; eval /quote & semi-quote
;; how to write your REPL
;; how to manage data attitude
