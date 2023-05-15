(load "sdf/manager/load")
(manage 'new 'user-defined-types)
(begin
(define connection)
(define game-in-session #f)
(define (find-keyword-function post-body)
  (let ((equal-index (string-search-forward "=" post-body)))
    (substring post-body 0 equal-index)))

(define (replace-all string substring-to-replace new-substring)
  (let loop ((string-so-far string))
    (let ((idx (string-search-forward substring-to-replace string-so-far)))
      (if (false? idx)
	  string-so-far
	  (loop (string-append (substring string-so-far 0 idx)
			       new-substring
			       (substring string-so-far (+ idx (string-length substring-to-replace)))))))))
					  
(define (decode-html html)
  (let* ((replace-space (replace-all html "+" " "))
	 (replace-double-quote (replace-all replace-space "%22" "\""))
	 (replace-hash-mark (replace-all replace-double-quote "%23" "#"))
	 (replace-dollar-sign (replace-all replace-hash-mark "%24" "$"))
	 (replace-percent (replace-all replace-dollar-sign "%25" "%"))
	 (replace-ampersand (replace-all replace-percent "%26" "&"))
	 (replace-plus (replace-all replace-ampersand "%2B" "+"))
	 (replace-comma (replace-all replace-plus "%2C" ","))
	 (replace-less-than (replace-all replace-comma "%3C" "<"))
	 (replace-equal (replace-all replace-less-than "%3D" "="))
	 (replace-greater-than (replace-all replace-equal "%3E" ">"))
	 (replace-forward-slash (replace-all replace-greater-than "%2F" "/"))
	 (replace-colon (replace-all replace-forward-slash "%3A" ":"))
	 (replace-question (replace-all replace-colon "%3F" "?"))
	 (replace-exclamation (replace-all replace-question "%21" "!"))
	 (replace-quote (replace-all replace-exclamation "%27" "'")))
    replace-quote))

(define (acquire-GET-name request-string)
  (let* ((name-index (substring-search-forward "name=" request-string))
	 (slash-index (string-search-forward "/" request-string))
	 (space-index (substring-search-forward " " request-string name-index (string-length request-string))))
    (if name-index
	(if space-index
	    (substring request-string (+ name-index 5) space-index)
	    (substring request-string (+ name-index 5))))))

;;; HTML Request

(define (request? object)
  (string? object))
(register-predicate! request? 'request)

(define handle-request
  (most-specific-generic-procedure 'handle-request 2 #f))

;;; GET Request
(define get-request:header
  (make-property 'header
		 'predicate string?))

(define get-request?
  (make-type 'get-request (list get-request:header)))
(set-predicate<=! get-request? request?)

(define make-get-request
  (type-instantiator get-request?))

(define (create-get-request header)
  (make-get-request 'header header))
		     
(define get-header
  (property-getter get-request:header get-request?))

(define-generic-procedure-handler handle-request
  (match-args get-request? port?)
  (lambda (pr client)
    (let ((request (get-header pr)))
      (if (not (false? (string-search-forward "name" request)))
	  (let ((name (string->symbol (decode-html (acquire-GET-name request)))))
	    (if (not (avatar-exists name))
		(start-web-adventure name client)
		(load-log name client)))))))
  
;;; POST Request

(define post-request:body
  (make-property 'body
		 'predicate string?))

(define post-request:name
  (make-property 'name
		 'predicate string?))

(define post-request?
  (make-type 'post-request (list post-request:body post-request:name)))
(set-predicate<=! post-request? request?)

(define make-post-request
  (type-instantiator post-request?))

(define (create-post-request body name)
  (make-post-request 'body body
		     'name name))
		     
(define get-body
  (property-getter post-request:body post-request?))

(define get-post-name
  (property-getter post-request:name post-request?))

(define-generic-procedure-handler handle-request
  (match-args post-request? port?)
  (lambda (pr client)
    (let* ((post-body (get-body pr))
	   (keyword-function (find-keyword-function post-body))
	   (avatar-name (get-post-name pr)))
      (cond
       ((string=? keyword-function "go-web")
	(let* ((go-web-index (string-search-forward "go-web=" post-body))
	       (direction (substring post-body (+ go-web-index 7))))
	  (go-web (string->symbol direction)
		  avatar-name
		  client)))
       ((string=? keyword-function "hangout")
	(let ((hangout-index (string-search-forward "hangout=" post-body)))
	  (hangout-web avatar-name
		       (string->number (substring post-body (+ hangout-index 8)))
		       client)))
						 
	((string=? keyword-function "take-thing")
	 (let ((take-thing-index (string-search-forward "take-thing=" post-body)))
	   (take-thing-web (string->symbol (substring post-body (+ take-thing-index 11)))
			   avatar-name
			   client)))
	((string=? keyword-function "drop-thing")
	 (let ((drop-thing-index (string-search-forward "drop-thing=" post-body)))
	   (drop-thing-web (string->symbol (substring post-body (+ drop-thing-index 11)))
			   avatar-name
			   client)))
	((string=? keyword-function "say")
	 (let* ((say-index (string-search-forward "say=" post-body))
		(decoded-html (decode-html (substring post-body (+ say-index 4)))))
	   (say-web avatar-name
		    client
		    (list decoded-html))))
	((string=? keyword-function "look-in-bag")
	 (let* ((look-in-bag-index (string-search-forward "look-in-bag=" post-body))
		(look-in-bag-string (substring post-body (+ look-in-bag-index 12))))
	   (if (eqv? (string-length look-in-bag-string) 0)
	       (look-in-bag avatar-name
			    client)
	       (look-in-bag avatar-name
			    client
			    (string->symbol look-in-bag-string)))))
	((string=? keyword-function "display-health")
	 (let* ((display-health-index (string-search-forward "display-health" post-body)))
	   (display-health avatar-name client)))
	((string=? keyword-function "fight")
	 (let* ((fight-index (string-search-forward "fight=" post-body))
		(troll-name (string->symbol (substring post-body (+ fight-index 6)))))
	   (fight avatar-name troll-name client)))
	(else (display "function not found"))))))

(define (run port)
  (let ((socket (open-tcp-server-socket port (host-address-any))))
    (set! connection socket)
    (accept-loop socket)))

(define (close-connection)
  (close-tcp-server-socket connection))

(define (find-content-length request-string)
  (let ((content-length-index (string-search-forward "Content-Length: " request-string))
	(cache-control-index (string-search-forward "Cache-Control: " request-string)))
    (substring request-string (+ content-length-index 16) cache-control-index)))

(define (substring-post-request? request)
  (string=? (substring request 0 4) "POST"))

(define (accept-loop socket)
  (let* ((client (tcp-server-connection-accept socket #t #f))
	 (request (read-total-request client ""))
	 (request-path (acquire-GET-file request)))
    (load-page client request-path request)
    (close-port client)
    (accept-loop socket)))

(define (load-page client request-file request)
  (cond ((string=? "" request-file) (load-page client "index.html" request))
	((not (file-exists? request-file)) (display "HTTP/1.0 404 \n\nFile Not Found (404)" client))
	(else (write-response client request-file request))))

(define (read-total-request client response)
  (let ((line (read-line client)))
      (if (string=? "" line)
	  response
	  (read-total-request client (string-append response line)))))

(define (acquire-GET-file request-string)
  (let* ((slash-index (string-search-forward "/" request-string))
	 (space-index (substring-search-forward " " request-string slash-index (string-length request-string)))
	 (question-index (substring-search-forward "?" request-string slash-index space-index)))
    (if question-index
	(substring request-string (+ slash-index 1) question-index)
	(substring request-string (+ slash-index 1) space-index))))

(define (acquire-GET-path request-string)
  (let* ((slash-index (string-search-forward "/" request-string))
	 (space-index (substring-search-forward " " request-string slash-index (string-length request-string))))
    (substring request-string (+ slash-index 1) space-index)))

(define (write-response client request-path request)
  (begin
    (display "HTTP/1.0 200 OK\n\n" client)
    (send-file client request-path request)))

(define (send-file client filename request)
   (let ((file (open-input-file filename)))
    (let loop ((ch (read-char file)))
      (if (eof-object? ch)
	  '()
	  (begin
	    (display ch client)
	    (loop (read-char file))))))
  (if (not game-in-session) ; handle starting a game
      (begin (set! game-in-session #t)
	     (start-web-world)))
  (if (substring-post-request? request) ; handle get & post request for game
      (let* ((content-length (string->number (find-content-length request)))
	     (post-body (read-string content-length client))
	     (avatar-name-symbol (string->symbol (acquire-GET-name request))))
	(let ((pr (create-post-request post-body avatar-name-symbol)))
	  (handle-request pr client)))
      (let ((gr (create-get-request request)))
	(handle-request gr client))))
)

(run 1234)
#| Navigate to localhost:1234 |#
#| Submit your name, it should redirect you to another page |#

#| When you're done with the server, C-c C-c in the REPL and run the command below |#
(close-connection)
