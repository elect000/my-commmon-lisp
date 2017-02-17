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

(defun start-over()
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
