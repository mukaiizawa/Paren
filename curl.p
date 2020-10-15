; curl

(<- $usage
"
Usage: paren curl.p URL
	Get http request in URL.
")

(function default-port (proto)
  (if (memeq? proto "http") 80
      (error "unsupported protocol. " proto)))

(function http-request (host port method uri version)
  ; RFC 2616
  ; Request-Line = Method SP Request-URI SP HTTP-Version CRLF
  (let (line nil)
    (with-client-socket (in out (client-socket host port))
      (.write-mem out (string method " " uri " " version "\r\n"))
      (.write-mem out "\r\n")
      (.flush out)
      (foreach (f (x) (.write-line $stdout x))
               (collect (f () (.read-line in)))))))

(function! main (args)
  (catch (Error (f (e) (write-line $usage) (throw e)))
    (if (nil? (cdr args)) (error "too few arguments.")
        (with-memory-stream ($in (cadr args))
          (let (ar (.new AheadReader) proto nil host nil port nil)
            (while (memneq? (.next ar) ":") (.get ar))
            (<- proto (.token ar))
            (.skip ar) (.skip ar "/") (.skip ar "/")    ; skip ://
            (while (&& (.next ar) (memneq? (.next ar) "/") (memneq? (.next ar) ":")) (.get ar))
            (<- host (.token ar))
            (when (memeq? (.next ar) ":")
              (.skip ar)
              (<- port (.skip-uint ar)))
            (if (memeq? (.next ar) "/") (.get ar)
                (.put ar "/"))
            (while (.next ar) (.get ar))
            (http-request host (|| port (default-port proto)) "GET" (.token ar) "HTTP/1.0"))))))
