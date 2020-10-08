; curl

(function usage ()
  (write-line "
Usage: paren curl.p URL
Get http request in URL.")
    (quit))

(function default-port (proto)
  (if (string= proto "http") 80
      (string= proto "https") 443
      (error "unexpected protocol " proto)))

(function http-request (host port method uri version)
  ; RFC 2616
  ; Request-Line = Method SP Request-URI SP HTTP-Version CRLF
  (let (line nil)
    (with-client-socket (in out (client-socket host port))
      (.write-bytes out (string method " " uri " " version "\r\n"))
      (.write-bytes out "\r\n")
      (.flush out)
      (.write-lines $stdout (.read-lines in)))))

(function! main (args)
  (catch (Error (lambda (e)
                  (write-line (.to-s e))
                  (write (.stack-trace e))
                  (usage)))
    (if (nil? (cdr args)) (error "too few arguments.")
        (with-memory-stream ($in (cadr args))
          (let (ar (.new AheadReader) proto nil host nil port nil)
            (while (string/= (.next ar) ":") (.get ar))
            (<- proto (.token ar))
            (.skip ar) (.skip ar "/") (.skip ar "/")    ; skip ://
            (while (&& (.next ar) (string/= (.next ar) "/") (string/= (.next ar) ":")) (.get ar))
            (<- host (.token ar))
            (when (string= (.next ar) ":")
              (.skip ar)
              (<- port (.skip-uint ar)))
            (if (string= (.next ar) "/") (.get ar)
                (.put ar "/"))
            (while (.next ar) (.get ar))
            (http-request host (|| port (default-port proto)) "GET" (.token ar) "HTTP/1.0"))))))
