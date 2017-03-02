;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; web server ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; warning : it needs apache2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun http-char (c1 c2 &optional (default #\space))
  (let ((code (parse-integer
	       (coerce (list c1 c2) 'string)
	       :radix 16
	       :junk-allowed t)))
    (if code
	(code-char code)
	default)))

(defun decode-param (s)
  (labels ((f (lst)
	     (when lst
	       (case (car lst)
			  (#\% (cons (http-char (cadr lst) (caddr lst))
				     (f (cdddr lst))))
			  (#\+ (cons #\space (f (cdr lst))))
			  (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

;; (decode-param "foo")
;; (decode-param "foo%3F")
;; (decode-param "foo+bar")

(defun parse-params (s)
  (let((i1 (position #\= s))
       (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))

;; (parse-params "name=bob&age=25&gender=male")

(defun parse-url (s)
  (let* ((url (subseq s
		      (+ 2 (position #\space s))  ;; if you don't get correct result, but this is correct
		      (position #\space s :from-end t)))
	 (x (position #\? url)))
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
	(cons url '()))))

;; (parse-url "GET \lolcats.html HTTP/1.1")
;; (parse-url "GET \lolcats.html?extra-funny=yes HTTP/1.1")

(defun get-header (my-stream)
  (let* ((s (read-line my-stream))
	 (h (let ((i (position #\: s)))
	      (when i	
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header my-stream)))))

;; (get-header (make-string-input-stream "foo: 1
;; bar: abc, 123
;;
;; "))


(defun get-content-params  (my-stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length)))) ;; empty string box of 'length' length
	(read-sequence content my-stream) 		    ;; add into the box from my-stream
	(parse-params content)))))			    ;; change html string -> accociate list

(defun serve (request-handler)
  (let ((socket (socket-server 8080))) 	;; open socket with port 8080
    (unwind-protect			;; guarantee of closing socket
	 (loop (with-open-stream (my-stream (socket-accept socket)) ;; guarantee of closing stream
		 (let* ((url (parse-url (read-line my-stream)))
			(path (car url))
			(header (get-header my-stream))
			(params (append (cdr url)
					(get-content-params my-stream header)))
			(*standard-output* my-stream)) 		    ;; prevent create dump string
		   (funcall request-handler path header params))))
      (socket-server-close socket))))

(defun hello-request-handler (path header params)
  (declare (ignore header))
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
	(if (not name)
	    (progn
              (princ "HTTP/1.1 200 OK")
              (terpri)
              (terpri)
	      (princ "<html><form>What is your name?<input name = 'name' />
</form></html>"))
	     (progn
	       (princ "HTTP/1.1 200 OK")
	       (terpri)
	       (terpri)
	       (format t "<html>Nice to meet you, ~a!</html>" (cdr name)))))
      (princ "Sorry... I don't know that page.")))


;; (hello-request-handler "lolcats" '() '())
;; (hello-request-handler "greeting" '() '())
;; (hello-request-handler "greeting" '() '((name . "bob")))
