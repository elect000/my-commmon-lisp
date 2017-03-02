;; CLISP特有のソケットコマンド・バイト/文字列変換コマンドを使っている

;; 日本語を利用するためのエンコーディング指定
(setf *terminal-encoding* charset:utf-8)
(setf *default-file-encoding* charset:utf-8)

;; 16進数で表されたASCIIコードをデコードする
(defun http-char (c1 c2 &optional (default #\space))
  (let ((code (parse-integer
	       (coerce (list c1 c2) 'string)
	       :radix 16
	       :junk-allowed t)))
    ;; 数値が帰ってきたらそれはASCIIコードなので、コードに対応する文字を得る。
    (if code
	(code-char code)
        default)))

;; http-charの日本語対応版
;; この#.ってなんだろう？
(defun http-byte (c1 c2 &optional (default #.(char-code #\space)))
  (let ((code (parse-integer
	       (coerce (list (code-char c1) (code-char c2)) 'string)
	       :radix 16
	       :junk-allowed t)))
    (or code default)))

;; URLエンコードのデコード
(defun decode-param (s)
  (labels ((f (lst)
	      (when lst
		(case (car lst)
		  ;; %であれば2桁の16進数で表示されるASCIIコードが続く
		  (#.(char-code #\%) (cons (http-byte (cadr lst) (caddr lst))
					   (f (cdddr lst))))
		  ;; +は空白文字
		  (#.(char-code #\+) (cons #.(char-code #\space) (f (cdr lst))))
		  ;; その他であれば、残りの文字列について再帰する
		  (t (cons (car lst) (f (cdr lst))))))))
    ;; 一旦バイト列としてデコードし、文字列へと変換しなおしている。
    (ext:convert-string-from-bytes
     (coerce (f (coerce (ext:convert-string-to-bytes s charset:utf-8) 'list))
	     'vector)
     charset:utf-8)))
    		  
;; リクエストパラメータをalistに変換する
;; e.g. name=bob&age=25&gender=male => ((NAME . "bob") (AGE . "25") (GENDER . "male"))
(defun parse-params (s)
  (let ((i1 (position #\= s))
	(i2 (position #\& s)))
    ;; i1が取得されていればkey=valueで文字列を切り出しconsする。
    ;; i2が取得されていれば更に次のkey=valueにたいして再帰する。
    (cond (i1 (cons (cons (intern (string-upcase (decode-param (subseq s 0 i1))))
			  (decode-param (subseq s (1+ i1) i2)))
		    (and i2 (parse-params (subseq s (1+ i2))))))
	  ((equal s "") nil)
	  (t s))))
;; TOOD 末尾再帰でもっと効率よく書ける

;; リクエストヘッダの最初の行を解析する
;; e.g. "GET /lolcats.html?extra-funny=yes HTTP/1.1" => ("lolcats.html" (EXTRA-FUNNY . "yes"))
(defun parse-url (s)
  (let* ((url (subseq s
		      (+ 2 (position #\space s))
		      (position #\space s :from-end t))) ;; from-endは末尾から探す
	 (x (position #\? url)))
    ;; #\?が見つかればそれ以降はリクエストパラメータなので、切り出してparse-paramsに渡す
    (if x
	(cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

;; リクエストヘッダの残りの行をalistに変換する
;; e.g. (CLでは"\n"はできない。代わりにそのまま改行した文字列を渡す)
;; "foo: 1
;;  bar: abc,123" => ((FOO . "1") (BAR . "abc,123"))
(defun get-header (stream)
  (let* ((s (read-line stream))
	 (h (let ((i (position #\: s)))
	      (when i
		(cons (intern (string-upcase (subseq s 0 i)))
		      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))
	    
;; リクエストボディの解析
;; リクエストヘッダからcontent-lengthを取得して、そのbyte長分streamから読み込みparse-paramsへ渡す。
(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
	(read-sequence content stream)
	(parse-params content)))))

;; サーバ
(defun serve (request-handler)
  (let ((socket (socket-server 8080)))
    (unwind-protect
      (loop (with-open-stream (stream (socket-accept socket))
	      (let* ((url    (parse-url (read-line stream)))
		     (path   (car url))
		     (header (get-header stream))
		     (params (append (cdr url)
				     (get-content-params stream header)))
		     (*standard-output* stream))
		;; 補助関数を使って取得したパラメータを引数にしてリクエストハンドラを実行
		(funcall request-handler path header params))))
      (socket-server-close socket))))
			      
;; リクエストハンドラのサンプル
;; この関数単体をREPLでテストできる。
(defun hello-request-handler (path header params)
  (princ header)
  (if (equal path "greeting")
      (let ((name (assoc 'name params)))
	(if (not name)
	    (princ "<html><body><form>What is your name?<input name='name' /></form></body></html>")
	    (format t "<html><body>Nice to meet you, ~a!</body></html>" (cdr name))))
      (princ "Sorry... I don't know that page.")))
