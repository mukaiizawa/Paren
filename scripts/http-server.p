; simple http server.

(import :datetime)
(import :html)
(import :http)
(import :optparse)
(import :sock)

(<- $context-root (path (getcwd)))

(function uri->local-path (uri)
  (if (! (.readable? (<- uri (car (split uri "?"))
                         uri (car (split uri "#"))
                         uri (.resolve $context-root (http.decode-url uri))))) (throw '(404))
      uri))

(function local-path->uri (local-path)
  (let (uri (.to-s (.relativize $context-root local-path)))
    (http.encode-url (str "/" uri (if (.dir? local-path) "/")))))

(function make-html (title body)
  (with-memory-stream ($out)
    (html.write
      `(html (:lang "ja")
             (head ()
                   (meta (:charset "UTF-8"))
                   (title () ,title))
             (body ()
                   (h1 () ,title)
                   (hr ())
                   ,@body
                   (hr ()))))))

(function handle-file (req res file)
  (.status-code! res 200)
  (.set-content-type-from-file res file)
  (.set-body res (.contents file)))

(function handle-dir (req res dir)
  (let (index.html (.resolve dir "index.html"))
    (if (.file? index.html) (handle-file req res index.html)
        (let (relative (.relativize $context-root dir))
          (.status-code! res 200)
          (.set-header res "Content-Type" "text/html")
          (.set-body res (make-html (format "Directory listing for %s" (.to-s relative))
                                    `((ul ()
                                          ,@(if (!= relative (path "."))
                                                `((li () (a (:href ,(local-path->uri (.parent dir))) ".."))))
                                          ,@(map (f (x)
                                                   `(li () (a (:href ,(local-path->uri x))
                                                              ,(str (.name x) (if (.dir? x) "/")))))
                                                 (.children dir))))))))))

(function handle-error (req res status-code message)
  (.status-code! res status-code)
  (.reason! res message)
  (.set-body res (make-html "Error response"
                            `((p () ,(str "Error code: " (.status-code res)))
                              (p () ,(str "Message: " (.reason res)))))))

(function handle (stream)
  (unwind-protect
    (let (req (.new HTTPRequest) res (.new HTTPResponse) dt (datetime.from-unix-time (time)))
      (catch
        (let (file (uri->local-path (.uri (.parse req stream))))
          (if (! (in? (.method req) '("GET" "HEAD"))) (throw (list 501 (format "Unsupported method '%s'" (.method req))))
              (.dir? file) (handle-dir req res file)
              (.file? file) (handle-file req res file)
              (throw '(404))))
        (f (e)
          (if (is-a? e Error) (throw e)
              (let ((code :opt message) (if (cons? e) e '(500)))
                (handle-error req res code message)))))
      ;; Set response headers.
      (.version! res "HTTP/1.0")
      (.set-header res "Server" "Paren HTTP server.")
      (.set-date-header res dt)
      (.set-header res "Content-Length" (byte-len (.body res)))
      (.write-start-line stream res)
      (.write-headers stream res)
      (if (= (.method req) "GET") (.write-body stream res))
      ;; Common Log Format.
      (printf "- - - [%02d/%3.3s/%04d:%02d:%02d:%02d +0000] %s %d %d\n"
              (.day dt) (.to-s.month dt) (.year dt) (.hour dt) (.minute dt) (.second dt)
              (.request-line req) (.status-code res) (.header res "Content-Length")))
    (.close stream)))

(function! main (args)
  (let ((op args) (.parse (.init (.new OptionParser) "p:") args)
                  root (car args) port (.get-int op "p" 8080))
    (if (&& root (! (.dir? (<- $context-root (.resolve $context-root root))))) (raise ArgumentError (format "invalid context root: %s" (.to-s $context-root)))
        (with-server-socket (server-sock port)
          (printf "Serving HTTP on http://127.0.0.1:%d/\n" port)
          (loop
            (handle (.init (.new HTTPStream) (accept server-sock))))))))
