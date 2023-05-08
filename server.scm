(load "sdf/manager/load")
(manage 'new 'user-defined-types)
(begin
(define connection)
(define game-in-session #f)

(define (run port)
  (let ((socket (open-tcp-server-socket port)))
    (set! connection socket)
    (accept-loop socket)))

(define (close-connection)
  (close-tcp-server-socket connection))

(define (find-content-length request-string)
  (let ((content-length-index (string-search-forward "Content-Length: " request-string))
	(cache-control-index (string-search-forward "Cache-Control: " request-string)))
    (substring request-string (+ content-length-index 16) cache-control-index)))

(define (post-request? request)
  (string=? (substring request 0 4) "POST"))

(define (accept-loop socket)
  (let* ((client (tcp-server-connection-accept socket #t #f))
	 (request (read-total-request client ""))
	 (request-path (acquire-GET-file request)))
    (handle-request client request-path request)
    (close-port client)
    (accept-loop socket)))

(define (handle-request client request-file request)
  (cond ((string=? "" request-file) (handle-request client "index.html" request))
	((not (file-exists? request-file)) (display "HTTP/1.0 404 \n\nFile Not Found (404)" client))
	(else (write-response client request-file request))))

(define (read-total-request client response)
  (let ((line (read-line client)))
      (if (string=? "" line)
	  response
	  (read-total-request client (string-append response line)))))

(define (acquire-GET-name request-string)
  (let* ((name-index (substring-search-forward "name=" request-string))
	 (slash-index (string-search-forward "/" request-string))
	 (space-index (substring-search-forward " " request-string name-index (string-length request-string))))
    (if name-index
	(if space-index
	    (substring request-string (+ name-index 5) space-index)
	    (substring request-string (+ name-index 5))))))

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
  (if (not (false? (string-search-forward "name" request))) ; handle new avatar
      (let ((name (string->symbol (acquire-GET-name request))))
	(if (not (avatar-exists name))
   	    (start-web-adventure name client))))
  (if (post-request? request) ; handle movement (ideally this is a new function
      (let* ((content-length (string->number (find-content-length request)))
	     (post-body (read-string content-length client))
	     (go-web-index (string-search-forward "go-web=" post-body))
	     (take-thing-index (string-search-forward "take-thing=" post-body)))
	(if (not (false? go-web-index))
	    (let ((direction (substring post-body (+ go-web-index 7))))
	      (go-web (string->symbol direction) (string->symbol (acquire-GET-name request)) client))
	    (if (not (false? take-thing-index))
		(begin
		(take-thing-web (string->symbol (substring post-body (+ take-thing-index 11)))
				(string->symbol (acquire-GET-name request))
				client)))
	    ))))
)
(run 1234)

#| Navigate to localhost:1234 |#
#| Submit your name, it should redirect you to another page |#

#| When you're done with the server, C-c C-c in the REPL and run the command below |#
(close-connection)
