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

(define (accept-loop socket)
  (let* ((client (tcp-server-connection-accept socket #t #f))
	(request (read-total-request client ""))
	(request-path (acquire-GET-path request)))
    (display request-path)
    (newline)
    (handle-request client request-path)
    (close-port client)
    (accept-loop socket)))

(define (handle-request client request-path)
  (cond ((string=? "" request-path) (handle-request client "index.html"))
	((not (file-exists? request-path)) (display "HTTP/1.0 404 \n\nFile Not Found (404)" client))
	(else (write-response client request-path))))

(define (read-total-request client response)
  (let ((line (read-line client)))
    (display (string-append response line))
      (if (string=? "" line)
	  response
	  (read-total-request client (string-append response line)))))

(define (acquire-GET-path request-string)
  (let* ((slash-index (string-search-forward "/" request-string))
	 (space-index (substring-search-forward " " request-string slash-index (string-length request-string)))
	 (question-index (substring-search-forward "?" request-string slash-index space-index)))
    (display question-index)
    (if question-index
	(substring request-string (+ slash-index 1) question-index)
	(substring request-string (+ slash-index 1) space-index))))

(define (write-response client request-path)
  (begin
    (display "HTTP/1.0 200 OK\n\n" client)
     (send-file client request-path)))

(define (send-file client filename)
  (let ((file (open-input-file filename)))
    (let loop ((ch (read-char file)))
      (if (eof-object? ch)
	  '()
	  (begin
	    (display ch client)
	    (loop (read-char file))))))
  (if (equal? "index.html" filename)
      (begin (set! game-in-session #t)
	     (start-web-adventure 'test client))))
)
(run 1234)

#| Navigate to localhost:1234 |#
#| Submit your name, it should redirect you to another page |#

#| When you're done with the server, C-c C-c in the REPL and run the command below |#
(close-connection)
