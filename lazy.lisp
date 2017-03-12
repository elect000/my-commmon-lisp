;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; lazy programing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro lazy (&body body)
  (let ((forced (gensym)) ;; unbrock name
	(value (gensym)))  ;; unblock name ;; created at function-called ;; it stored after called it.
	`(let ((,forced nil) ;; created at this compiled/declared
	       (,value nil))
	   (lambda ()			;; if funcall 
	     (unless ,forced 		;; it was calculated?
	       (setf ,value (progn ,@body)) ;; calculate it
	       (setf ,forced t)) 	    ;; it was calculated
	     ,value))))		;; it had been calculated, return it.

(defun force (lazy-value)
  (funcall lazy-value)) ;; call lambda-func


(defun add (a b) (princ "I am adding now") (+ a b))

(defparameter *foo* (lazy (add 1 2)))

(force *foo*)

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defparameter *integers*
  (labels ((f (n)
	     (lazy-cons n (f (1+ n)))))
    (f 1)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(lazy-cdr *integers*)

;; #<FUNCTION :LAMBDA NIL
;; (UNLESS #:G3951 (SETF #:G3952 (PROGN (CONS N (F (1+ N))))) (SETF #:G3951 T))
;; #:G3952>

;; because ...
;; *integers* = (lazy-cons 1 (lazy-cons 2 (lazy-cons 3 (lazy-cons ...))))

(defun make-lazy (lst)
  (lazy (when lst 
	  (cons (car lst) (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(take 10 *integers*)

(take 10 (make-lazy '(q w e r t y u i o p a s d f)))

(take-all (make-lazy '(q w e r t y u i o p a s d f)))

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
	  (cons (funcall fun (lazy-car lst))
		(lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur) ;; current element
	     (if (lazy-null lst-cur) ;; it's nil (not match the conditions)
		 (force (lazy-mapcan fun (lazy-cdr lst))) ;; deal cdr-list
		 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur))))))) ;;
    (lazy (unless (lazy-null lst)
	    (f (funcall fun (lazy-car lst))))))) 
;; this func must return nil in no match element

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    	(let ((x (lazy-car lst)))
	  (if (funcall fun x)
	      x
	      (lazy-find-if fun (lazy-cdr lst))))))
;; this func must return nil in no match element

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))

(take 10 (lazy-mapcar #'sqrt *integers*))

(take 10 (lazy-mapcan (lambda (x)
			(if (evenp x)
			    (make-lazy (list x));; lazy list in one element
			    (lazy-nil)))	;;  - lazy cannot make element
		      *integers*))

(lazy-find-if #'oddp (make-lazy '(2 4 6 7 8 10)))

(lazy-nth 4 (make-lazy '(a b c d e f g))) ;; E

