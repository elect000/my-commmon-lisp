;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; elect000 ;;; email : e.tmailbank@gmail.com ;;;;;;;;;;;;;;;;;;;;;;
;; stream ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; stream ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; - console stream
;; - file stream
;; - socket stream
;; - string stream
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; output stream
;; ability
;; - check wheather it is output stream or not
;; - send as output stream

(output-stream-p *standard-output*) ;; T

(write-char #\x *standard-output*) ;; x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; input stream
;; ability
;; - check wheather it is input stream or not
;; - get "a" element of input stream

(input-stream-p *standard-input*) ;; T

(read-char *standard-input*) ;; #\1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; write to file ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :input)
  (read my-stream))

(let ((animal-noises '((dog . woof)
		       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))
;; ((dog .woof) (cat .meow))

(with-open-file (my-stream "data.txt" :direction :output :if-exists :error)
  (print "my data" my-stream))
;; error 'file is already exist'

(with-open-file (my-stream "data.txt" :direction :output :if-exists :supersede)
  (print "my-data" my-stream))

;; with-open-file promise correct closing of file
;; with-open-file -> make file-hundle 
;; 		  -> add value into the file-hundle
;;		  -> do something with it
;;                -> kill the file-hundle

;; use socket ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; socket address ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; two elements : IP address / port number ;;
;; connection : server <---> client
;;  server ... listen
;;  client ... make connection with server and send message
;;
;; this codes compiled in clisp

;; server
(defparameter my-socket (socket-server 38533)) ; on my-server
(defparameter my-stream (socket-accept my-socket))


;; client
(defparameter my-stream (socket-connect 38533 "127.0.0.1")) 
;; 127.0.0.1 is always mean my-PC

;; client
(print "Yo Server!" my-stream)

;; server
(read my-stream)

;; server
(print "What up, Client!" my-stream)

;; client
(read my-stream)

;; client and server
(close my-stream)

;; string stream ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defparameter foo (make-string-output-stream))

(princ "This will go into foo. " foo)

(princ "This will also go into foo. " foo)

(get-output-stream-string foo) ;; "This will go into foo. This will also  go into foo."

;; this function will be used in too large length of string.
(with-output-to-string (*standard-input*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 5 2))) ;; the sum of 5 and 2 is 7

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; review ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; stream 
;;  - console stream
;;  - file stream
;;  - socket stream
;;  - string stream
;; stream direction
;;  - output
;;  - input
;; 
;; tips ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; with-open-file					;;
;;  :if-exists						;;
;;      :error						;;
;;	:new-version ;; a-ver1.some , a-ver2.some	;;
;;      :rename						;;
;;  	:overwrite ;; B A ...				;;
;;      :append    ;; A B ...				;;
;;      :supersede					;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; clisp/konalinux
;; clisp 2.49 download in document folder and install as its webpage
;;
;; later, you see this error
;;
;;Please install libsigsegv like this:
;;  CC='gcc -Wall'; export CC
;;  mkdir tools; cd tools; prefix=`pwd`/x86_64-unknown-linux-gnu
;;  wget http://ftp.gnu.org/pub/gnu/libsigsegv/libsigsegv-2.8.tar.gz
;;  tar xfz libsigsegv-2.8.tar.gz
;;  cd libsigsegv-2.8
;;  ./configure --prefix=${prefix} && make && make check && make install
;;  cd ../..
;;  rm -f ./config.cache
;;  ./configure --with-libsigsegv-prefix=${prefix} --srcdir=../ --prefix=/usr --docdir=/usr/share/doc/clisp-2.49 --with-libsigsegv-prefix=/usr
;;If you insist on building without libsigsegv, please pass
;;  --ignore-absence-of-libsigsegv
;;to this script:
;;  ./configure --ignore-absence-of-libsigsegv --srcdir=../ --prefix=/usr --docdir=/usr/share/doc/clisp-2.49 --with-libsigsegv-prefix=/usr
;;If you have installed libsigsegv, but clisp does not detect it,
;;you might have installed it incorrectly, see section 2 in in unix/INSTALL.
;;
;; First:
;; write in your .bashrc
;;   export CLISP_HOME= "the state of your clisp-2.49"
;;   export CLISP_EXE=/usr/bin/clisp.bin
;;   export PATH = $CLISP_HOME:$CLISP_EXE:$PATH
;;
;; And also install libsegegv-dev (sudo apt install libsegegv-dev) 
;;
;; you should do as error log
;;  and install again
;;
;; last "sudo make install"
;;
;;
;;
;; after installed ...
;; remove CLISP_HOME and CLISP_EXE
