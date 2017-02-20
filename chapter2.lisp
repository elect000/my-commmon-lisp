;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; entrance : game "guess-my-number" ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *small* 1)
(defparameter *big* 100)

(defvar *foo* 5)
(defvar *foo* 6) ;; not reading because it's definding by defvar

(defun guess-my-number()
  (ash (+ *small* *big*) -1)) ;; ash: binary number managiment


(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun	bigger ()
  (setf *small* (1- (guess-my-number)))
  (guess-my-number))

(Defun start-over()
  (setf *small* 1)
  (setf	*big* 100)
  (guess-my-number))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; local value and function ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((a 5)
      (b 6))
  (+ a b))

(flet ((f (n)
	  (+ n 10)))
   (f 5))

(flet ((f (n)
	 (+ n 10))
       (g (n)
	 (- n 3)))
  (g (f 5))) 

(labels ((a (n)
	   (+ n 5))
	 (b (n)
	   (+ (a n) 6)))
  (b 10))  ;;  this is for using internal local function 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eq 'fooo 'fOoo) ;; -> T

(+ 1 1.0)        ;; -> 2.0

(expt 53 53)     ;; -> 2435......

(/ 4 6)          ;; -> 2/3

(/ 4.0 6)        ;; -> 0.66667

(princ "Tutti Frutti")

(princ "He yeiled \"stop tjat the\" from the busy street.")

(expt 2 3)  ;; -> code mode

'(expt 2 3) ;; -> data mode

(cons 'children 'cat)

(cons 'children 'nil)

(cons 'children ())

(cons 'pork '(beef children))

(cons 'beef '(cons 'chicken ()))

(cons 'pork (cons 'beef (cons 'chicken ())))

(car '(pork beef chicken))

(cdr '(pork beef chicken))

(car (cdr '(pork beef chicken)))

(cadr '(pork beef chicken))

(list 'pork 'beef 'chicken)

(car '((peas carrots tomatoes) (pork beef chicken)))

(cons (cons 'peas (cons 'carrots (cons 'tomatoes ()))) ;; (cons data lists) 
      (cons (cons 'pork (cons 'beef (cons 'chicken ()))) ()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; judge and condition ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun my-length (list)
  (if list
      (+ (my-length (cdr list)) 1)
      0))
;; nil 

(eq '() 'nil) ;;-> nil
(eq 'nil nil) ;;-> nil
(eq nil ())   ;;-> nil
(eq () '())   ;;-> nil


;; if ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (= (+ 1 2) 3)
    'yup
    'nope)

(if (oddp 5)
    'odd-number ;; read only this 
    (/ 1 0)) ;; does not read this

(defvar *number-was-odd* nil)


(if (oddp 5)   
    (progn (setf *number-was-odd* t)
	   'odd-number)
    'even-number)

;; when and unless ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *number-is-odd* nil)

(when (oddp 5)
  (setf *number-is-odd* t)
  'odd-number)

(unless (oddp 4)
  (setf *number-is-odd* nil)
  'even-number)

;; cond ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *arch-enemy* nil)

(defun pudding-eater (person)
  (cond ((eq person 'henry) 
	 	(setf *arch-enemy* 'stupid-lisp-alien)
	 	'(curse you lisp alien - you ate my pudding))
	((eq person 'johnny) 
	 	(setf *arch-enemy* 'useless-old-johnny)
	 	'(i hope you choked on my pudding johnny))
	(t      '(why you ate my pudding stranger ?))))

;; case ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun pudding-eater (person)
  (case person
    ((henry) 
     	(setf *arch-enemy* 'stupid-lisp-alien)
     	'(curse you lisp alien - you ate my pudding))
    ((johnny) 
     	(setf *arch-enemy* 'useless-old-johnny)
     	'(i hope you choked on my pudding johnny))
    (otherwise 
     	'(why you eat my pudding stranger ?))))

;; application ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(and (oddp 5) (oddp 7) (oddp 9))

(or (oddp 4) (oddp 7) (oddp 8))

(defparameter *is-it-even* nil)

(or (oddp 4) (setf *is-it-even* t)) ;; -> *is-it-even* : t

(defparameter *is-it-even* nil)

(or (oddp 5) (setf *is-it-even* nil)) ;; -> *is-it-even* : nil

(if (member 1 '(3 4 1 5))
    'one-is-in-the-list
    'one-is-not-in-the-list) ;; ->  one-is-in-the-list

(member 1 '(3 4 1 5)) ;; -> (1 5)

(if (member nil '(3 4 nil 5))
    'nil-is-in-the-list
    'nil-is-not-in-the-list) ;; -> nil-is-in-the-list

(member nil '(3 4 nil 5)) ;; -> (nil 5)

(if (find-if #'oddp '(2 4 5 6))
    'there-is-an-odd-number
    'there-is-not-an-odd-number) ;; -> there-is-an-odd-number

;; !!!warning!!! ;;
(find-if #'null '(nil 3 4 5)) ;; -> nil

;; comparison ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter *fruit* 'apple)

(cond ((eq *fruit* 'apple) 
       'its-an-apple)
      ((eq *fruit* 'orange) 
       'its-an-orange))

