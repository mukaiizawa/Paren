; curl

(function usage ()
  (error "Usage: curl url"))

(function default-port (proto)
  (if (string= proto "http") "80"
      (string= proto "https") "443"
      (error "unexpected protocol " proto)))

(function http-request (host port method uri version)
  ; RFC 2616
  ; Request-Line = Method SP Request-URI SP HTTP-Version CRLF
  (let (line nil)
    (with-client-socket (in out (client-socket host port))
      (write-bytes (string method " " uri " " version "\r\n") out)
      (write-bytes "\r\n" out)
      (flush out)
      (while (<- line (read-line in))
        (write-line line)))))

(function! main (args)
  (if (nil? (cdr args)) (usage)
      (let (url (cadr args) ar (.init (.new AheadReader) url) proto nil host nil port nil)
        (while (string/= (.next ar) ":") (.get ar))
        (<- proto (.token ar))
        (.skip ar) (.skip ar "/") (.skip ar "/")    ; skip ://
        (while (&& (.next ar) (string/= (.next ar) "/") (string/= (.next ar) ":")) (.get ar))
        (<- host (.token ar))
        (when (string= (.next ar) ":")
          (.skip ar)
          (while (&& (.next ar) (string/= (.next ar) "/")) (.get ar))
          (<- port (.token ar)))
        (if (string= (.next ar) "/") (.get ar)
            (.put ar "/"))
        (while (.next ar) (.get ar))
        (http-request host (|| port (default-port proto)) "GET" (.token ar) "HTTP/1.0"))))
