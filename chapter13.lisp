;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; web server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; error ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(error "hoge")

(define-condition foo () ()
  (:report (lambda (condition my-stream)
	     (declare (ignore condition))
	     (princ "Stop FOOing around, numbskull!" my-stream))))

(error 'foo) ;; -> Stop Fooing ...

(defun bad-function ()
  (error 'foo))

;; get right of error
(handler-case (bad-function)
	      (foo () "somebody signaled foo!")
	      (bar () "somebody signaled bar!"))
;; -> somebody signaled foo!

;; unexpected error
(unwind-protect (/ 1 0)
  (princ "I need to say 'flubyduby' matter what" ))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun http-char (c1 c2 &optional (default #\space)) ;; c1 , c2 are charactor / others change to #\space
  (let ((code (parse-integer			     ;; c1c2 as number
	       (coerce (list c1 c2) 'string)         ;; c1c2 as string
	       :radix 16		;; 16 decimal number
	       :junk-allowed t)))	;; not error but nil
   (if code
       (code-char code)			;; decode
       default)))			

(defun decode-paren (s)
  (labels ((f (lst)
	     (when lst
	       (case (car lst)
		 (#\% (cons (http-char (cadr lst) (caddr lst))  ;; %somesome... -> (decode somesome)
			    (f (cdddr lst)))) 
		 (#\+ (cons #\space (f (cdr lst))))		;; +... -> _ (#\space)
		 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))  ;; where is "="
	(i2 (position #\& s))) ;; where is "&"
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1))) ;; (inrern some) -> (type-of some) = symbol
			  (decode-paren (subseq s (1+ i1) i2)))    ;; decode value
		    (and i2 (parse-params (subseq s (1+ i2)))))) ;; (and T ?) -> (T T) / (and F ?) ->(F F)
	  ((equal s "") nil)
	  (t s))))

(defun parse-url (s) ;; ex. "GET \lolcats.html HTTP/1.1"
  (let* ((url (subseq s	(+ 2 (position #\space s))  ;;; ? 
		      	(position #\space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
	(cons url '()))))

(parse-url "GET /lolcats.html?extra-funny=yes HTTP/1.1") ;; -> ("lolcats.hetm" (EXTRA-FUNNY . "yes"))

(defun get-header (stream)
  (let* ((s (read-line stream))
	 (h (let ((i (position #\: s)))
	      (when i
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2)))))))
    (when h				;; h is dot list ex. (HOST . "www.my-website.com")
      (cons h (get-header stream)))))


;; !WARNING!
(get-header (make-string-input-stream "foo: 1
bar: abc, 123


")) ;; we need newline because lisp has not the key \n 
;; ex. HOST: www.mywebsite.com -> ((HOST . "www.my-website.com"))


(defun get-content-params (stream header)  ;; Get POST request
  (let ((length (cdr (assoc 'connect-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(parse-params content))))) ;;ex. "name=elect" -> (NAME . "elect")

(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect 
	 (loop (with-open-stream (my-stream (socket-accept socket)) ;; this is function of search request
		 (let* ((url (parse-url (read-line my-stream))) ;; ex. ("greeting" (EXTRA-FUNNY . "yes"))
			(path (car url)) ;; list -> string 
			(header (get-header my-stream)) 
			(params (append (cdr url)	;; url
					(get-content-params my-stream header))) ;; POST request
			(*standard-output* my-stream))
		   (funcall request-handler path header params))))
      (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (declare (ignore header))
  (if (equalp path "greeting")	
      (let ((name (assoc 'name params)))
	(if (not name)
	    (progn
              (princ "HTTP/1.1 200 OK")
              (terpri)
              (terpri)
	      (princ "<html><form>What is your name?<input name = 'name' /></form></html>"))
	    (progn
              (princ "HTTP/1.1 200 OK")
              (terpri)
              (terpri)
	    (format t "<html>Nice to meet you, ~a!</html>" (cdr name)))))
      (princ "Sorry... I don't know that page")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; error 		-> announce condition
;; handle-case 		-> get condition 
;; unwind-protect 	-> force running
;; web server 		.. deal with HTTP request
;; famous request	- GET 	... look at information
;;			- POST	... submit web form
;;	there are some request parameter which add after URL or in request body
;;
;;
;; request body
;;	POST request is included it
;;	ex.	POST /login.html HTTP/1.1   ... request header
;;		HOST: www.mywebsite.com
;;		...
;;		Connection: keep-alive
;;		Content-Length: 39
;;
;;		userid=foo&password=supersecretpassword ... request body which has contenst-length char
;;
;; Get request
;;      URL is included it
;;	ex. 	http://localhost.lolcats/search?q=dogs&hl=en&safe=off&...
;;						after ? , that is request parameter
;;
;; request parameter 
;;	ex.	"name=bob&age=25&..."
;;
;;
;;
;; about hello-request-handler
;;    		its third argument is alist such as ((name . "elect") (name . "hoge") (name . "foo"))
;;			it will return "elect" as name which is the latest request
;;		so if you enter the page with the address "http://127.0.0.1:8080/greeting?name=elect&name=sino"
;; 			, it will return "Nice to meet you, elect!"
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
