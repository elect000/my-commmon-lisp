;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; the magic of macro ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add (a b)
  (let ((x (+ a b)))
    (format t "The sum is ~a" x)
    x))


(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))


(let ((foo (+ 2 3)))
  (* foo foo))    
;; 25


(let1 foo (+ 2 3) (* foo foo))
;; 25

(let1 foo (+ 2 3) (princ "Lisp is awesome!")(* foo foo))
;; Lisp is awesome!
;; 25

(defun add2 (a b)
  (let1 x (+ a b)
    (format t "The sum is ~a" x)
    x))

(macroexpand '(let1 foo (+ 2 3)
	       (* foo foo)))

;; (LET ((FOO (+ 2 3))) (* FOO FOO)) 
;; T

(defun my-length (lst)
  (labels ((f (lst acc)
	     (if lst
		 (f (cdr lst) (1+ acc))
		 acc)))
    (f lst 0)))

(defmacro split (val yes no)
  `(if ,val
    (let ((head (car ,val))
	  (tail (cdr ,val)))
      ,yes)
    ,no))
;; this macro create some variable, so it is called anaphoric macro

(defun my-length (lst)
  (labels ((f (lst acc)
	     (split lst
		   (f tail (1+ acc))
		    acc)))
    (f lst 0)))

(split (progn (princ "Lisp rocks!")
	      '(2 3))
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))
;; Lisp rocks!Lisp rocks!Lisp rocks!This can be split into 2 and (3).

(macroexpand '(split (progn (princ "Lisp rocks!")
	      '(2 3))
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split.")))

;; (IF (PROGN (PRINC "Lisp rocks!") '(2 3))
;; (LET
;;  ((HEAD (CAR (PROGN (PRINC "Lisp rocks!") '(2 3))))
;;   (TAIL (CDR (PROGN (PRINC "Lisp rocks!") '(2 3)))))
;;  (FORMAT T "This can be split into ~a and ~a." HEAD TAIL))
;;  (FORMAT T "This cannot be split."))

(defmacro split (val yes no)
  `(let1 x ,val
     (if x
	 (let ((head (car x))
	       (tail (cdr x)))
	   ,yes)
	 ,no)))

(split (progn (princ "Lisp rocks!")
	      '(2 3))
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))

;; Lisp rocks!This can be split into 2 and (3).

(macroexpand '(split (progn (princ "Lisp rocks!")
	      '(2 3))
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split.")))

;; (LET ((X (PROGN (PRINC "Lisp rocks!") '(2 3))))
;;  (IF X
;;   (LET ((HEAD (CAR X)) (TAIL (CDR X)))
;;    (FORMAT T "This can be split into ~a and ~a." HEAD TAIL))
;;   (FORMAT T "This cannot be split.")))


;; (let1 x 100
;;  (split '(2 3)
;;	 (+ x head)
;;	 nil))
;; error


(macroexpand '(split '(2 3)
	       (+ x head)
	       nil))

;; (LET ((X '(2 3))) (IF X (LET ((HEAD (CAR X)) (TAIL (CDR X))) (+ X HEAD)) NIL))

(defmacro split (val yes no)
  (let1 g (gensym) ;; g create in read by lisp system
    `(let1 ,g ,val ;; this create in running
       (if ,g
	   (let ((head (car ,g))
		 (tail (cdr ,g)))
	     ,yes)
	   ,no))))

;; (macroexpand '(split '(1 2) (princ "hey") (princ "hoge")))
;; (LET ((#:G4082 '(1 2)))
;;  (IF #:G4082 (LET ((HEAD (CAR #:G4082)) (TAIL (CDR #:G4082))) (PRINC "hey"))
;;   (PRINC "hoge")))

;; (macroexpand '(split '(1 2) (princ "hey") (princ "hoge")))
;; (LET ((#:G4083 '(1 2)))
;;  (IF #:G4083 (LET ((HEAD (CAR #:G4083)) (TAIL (CDR #:G4083))) (PRINC "hey"))
;;   (PRINC "hoge")))

(defun pairs (lst)
  (labels ((f (lst acc)	
	     (split lst	;; lst -> (head tail)
		    (if tail
			(f (cdr tail) (cons (cons head (car tail)) acc)) 
			(reverse acc))
		    (reverse acc))))
    (f lst nil)))

;; (cdr tail) -> (head tail' (cdr tail))

;; (pairs '(a b c d e f g)) 
;; -> ((A . B) (C . D) (E . F))

(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
		,@body))
       (self ,@(mapcar #'cdr p)))))

(defun my-length (lst)
  (recurse (lst lst
		acc 0)
    (split lst			;; have data?
	   (self tail (1+ acc))	;; yes	-> continu to func = (self (lst acc) ...)
	   acc)))		;; no -> return acc (finished)

;;  (macroexpand '(recurse (n 9) 
;;		(fresh-line) 
;;		(if (zerop n) (princ "lift off!") (progn (princ n) (self (1- n))))))

;; (LABELS
;;  ((SELF (N) (FRESH-LINE)
;;    (IF (ZEROP N) (PRINC "lift off!") (PROGN (PRINC N) (SELF (1- N))))))
;;  (SELF 9))

(macroexpand '(recurse (lst lst acc 0) (split lst (self tail (1+ acc)) acc)))

;; (LABELS ((SELF (LST ACC) (SPLIT LST (SELF TAIL (1+ ACC)) ACC))) (SELF LST 0))


;; lst as lst
;;  0  as acc
;; 
;; (pairs vars) ... vars = '(lst lst acc 0)
;;
;; (pairs '((1 2 3 4) (1 2 3 4) acc 0))
;; (mapcar #'car *)
;; -> ((1 2 3 4) ACC)
;; (mapcar #'cdr **)
;; -> ((1 2 3 4) 0)
;;

;; disadvantage of macro
(defun my-length (lst)
  (reduce (lambda (x i)
	    (1+ x)
	    (princ i))
	  lst
	  :initial-value 0))

;; (my-length '(A B C D))
;; ABCD
;; 4

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; macro let us code more fast
;; macro remove our deja vu
;; macro has some danger problem 
;;  - run same code many times
;;  - macro make variable. it kills same name's variable 
;;    - when you prevent it, you use "gensym"
;; - macro's variable which we can use from outer layer, its macro is anaphoric macro
;; - functional-programing is more useful than macro, so you use functional-programing at first
